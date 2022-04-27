import sys
import sonnet as snt
import numpy as np
import os
from shutil import rmtree

import tensorflow.compat.v1 as tf
tf.enable_resource_variables()
tf.disable_eager_execution()

from meshgraphnets import common, normalization, dataset, core_model


class Model(snt.AbstractModule):
  '''
  A customized model adapted from cloth_model.py and run_model.py in the original meshgraphnets 
  code. This model does not use the same pre-processing functions and only supports the edges in 
  word space (as no contact is simulated). Besides, using few conditional statements, different 
  architectures can be generated. For example, the input_hf argument, assumes that the high-fidelity 
  inputs are passed, or the input_mat argument assumes that the material parameter is passed. Also, 
  the name argument defines that if it is a hybrid model (i.e, hml) or not.
  '''
  
  def __init__(self,
               output_size=6,
               latent_size=int(sys.argv[1]),
               num_layers=int(sys.argv[2]),
               message_passing_steps=int(sys.argv[3]), 
               edge_normalizer_size=3,
               input_hf=False,
               input_nodal_disp=False,
               input_fixed_nodes=False,
               input_mat=False,
               name='hml'):
    super(Model, self).__init__(name=name)

    learned_model = core_model.EncodeProcessDecode(output_size,
                                                   latent_size,
                                                   num_layers,
                                                   message_passing_steps)
    
    node_normalizer_size = 0
    
    if name=='hml':
      node_normalizer_size = 6
    else:
      input_nodal_disp = input_fixed_nodes = input_mat = True
    
    if input_nodal_disp == True: node_normalizer_size += 2

    if input_fixed_nodes == True: node_normalizer_size += 2

    if input_mat == True: node_normalizer_size += 1

    with self._enter_variable_scope():   # for Sonnet
      self._learned_model = learned_model
      self._output_normalizer = normalization.Normalizer(
          size=output_size, name='output_normalizer')
      self._node_normalizer = normalization.Normalizer(
          size=node_normalizer_size, name='node_normalizer')
      self._edge_normalizer = normalization.Normalizer(
          size=edge_normalizer_size, name='edge_normalizer')
      self.name = name
      self.input_hf = tf.constant(input_hf, dtype=tf.bool)
      self.input_nodal_disp = tf.constant(input_nodal_disp, dtype=tf.bool)
      self.input_fixed_nodes = tf.constant(input_fixed_nodes, dtype=tf.bool)
      self.input_mat = tf.constant(input_mat, dtype=tf.bool)
  
  def _build_graph(self, inputs, is_training):
    """Builds input graph."""
    ###### construct graph nodes ######
    node_features = tf.cond(tf.constant(self.name[0:3]=='hml', dtype=tf.bool),
                            lambda: tf.cond(self.input_hf,
                                            lambda: tf.cond(self.input_mat,
                                                            lambda: tf.concat([inputs['mat'],
                                                                               inputs['hf_res']],
                                                                                        axis=-1),
                                                            lambda: inputs['hf_res']),
                                            lambda: tf.cond(self.input_mat,
                                                            lambda: tf.concat([inputs['mat'],
                                                                               inputs['lf_res']],
                                                                                        axis=-1),
                                                            lambda: inputs['lf_res'])),
                            lambda: inputs['mat'])
    
    node_features = tf.cond(self.input_fixed_nodes,
                            lambda: tf.concat([node_features,
                                               tf.one_hot(inputs['fixed_nodes'][:, 0], 2)],
                                                                            axis=-1),
                            lambda: node_features)
    
    node_features = tf.cond(self.input_nodal_disp,
                            lambda: tf.concat([node_features,
                                               inputs['nodal_disp']],
                                                             axis=-1),
                            lambda: node_features)
    
    
    ###### construct graph edges ######
    senders, receivers = common.triangles_to_edges(inputs['cells'])
  
    # tf.gather is for choosing some elements from a larger group
    relative_world_pos = (tf.gather(inputs['world_pos'], senders) -
                          tf.gather(inputs['world_pos'], receivers))
    
    edge_features = tf.concat([
        relative_world_pos,
        tf.norm(relative_world_pos, axis=-1, keepdims=True),
        ], axis=-1)
    
    mesh_edges = core_model.EdgeSet(
        name='mesh_edges',
        features=self._edge_normalizer(edge_features, is_training),
        receivers=receivers,
        senders=senders)
    
    # this is a multi-edge interaction network with residual connections
    return core_model.MultiGraph(
        node_features=self._node_normalizer(node_features, is_training),
        edge_sets=[mesh_edges])
  
  def _build(self, inputs):
    graph = self._build_graph(inputs, is_training=False)
    per_node_network_output = self._learned_model(graph)
    return self._output_normalizer.inverse(per_node_network_output)
  
  @snt.reuse_variables
  def loss(self, inputs):
    """L2 loss on position."""
    graph = self._build_graph(inputs, is_training=True)
    network_output = self._learned_model(graph)
    target_position = inputs["hf_res"]
    target_normalized = self._output_normalizer(target_position)
  
    # build loss
    error = tf.reduce_sum((target_normalized - network_output)**2, axis=1)
    loss = tf.reduce_mean(error)
    return loss



def train(model,
          dataset_dir,
          checkpoint_dir,
          learning_rate,
          n_initial_steps=400,
          num_training_steps = 2001,
          save_checkpoint_secs = 30,
          start_afresh=True):
  
  if start_afresh==True and os.path.exists(checkpoint_dir): rmtree(checkpoint_dir)

  ds = dataset.load_dataset(dataset_dir, "train")
  ds = ds.flat_map(tf.data.Dataset.from_tensor_slices) # e.g., [1,2,3] ==>> [1],[2],[3] ...
  ds = ds.shuffle(10000,seed=40).repeat(None).prefetch(10)

  inputs = tf.data.make_one_shot_iterator(ds).get_next()
  loss_op = model.loss(inputs)
  global_step = tf.train.create_global_step()
  lr = tf.train.exponential_decay(learning_rate=learning_rate,
                                  global_step=global_step,
                                  decay_steps=int(5e6),
                                  decay_rate=0.1) + 1e-8
  optimizer = tf.train.AdamOptimizer(learning_rate=lr)
  train_op = optimizer.minimize(loss_op, global_step=global_step)
  # Don't train for the first few steps, just accumulate normalization stats
  train_op = tf.cond(tf.less(global_step, n_initial_steps),
                     lambda: tf.group(tf.assign_add(global_step, 1)),
                     lambda: tf.group(train_op))

  with tf.train.MonitoredTrainingSession(
      hooks=[tf.train.StopAtStepHook(last_step=num_training_steps)],
      checkpoint_dir=checkpoint_dir,
      save_checkpoint_secs=save_checkpoint_secs) as sess:
    lossList = []
    stepList = []
    while not sess.should_stop():
      _, step, loss = sess.run([train_op, global_step, loss_op])
      # print(step)
      # print(loss)
      if step % 100 == 0:
        print('Step %s: Loss %s\n'%(step, loss))
      lossList.append(loss)
      stepList.append(step)
    print('Training complete.')
  
  fdir = os.path.join(checkpoint_dir, 'train.npy')
  with open(fdir, 'wb') as f:
    np.save(f, np.array(lossList))
    np.save(f, np.array(stepList))


def eval(model,
         dataset_dir,
         checkpoint_dir,
         db_key="test"):
  
  ds = dataset.load_dataset(dataset_dir, db_key)

  inputs = tf.data.make_one_shot_iterator(ds).get_next()
  def evaluate(model, inputs):
    target = inputs['hf_res']
    initial_state = {k: v[0] for k, v in inputs.items()}
    prediction = model(initial_state)
    error = tf.reduce_mean((prediction - target)**2)
    return [error, target, prediction]
  ops = evaluate(model, inputs)
  tf.train.create_global_step()
  with tf.train.MonitoredTrainingSession(
      checkpoint_dir=checkpoint_dir,
      save_checkpoint_secs=None,
      save_checkpoint_steps=None) as sess:
    
    errorList = []
    targetList = []
    predictionList = []
  
    while not sess.should_stop():
      error, target, prediction = sess.run(ops)
      errorList.append(error)
      targetList.append(target)
      predictionList.append(prediction)
    
  print('Error List: %s'%(errorList))

  fdir=os.path.join(checkpoint_dir, 'evaluation_(%s).npy'%(db_key))
  with open(fdir, 'wb') as f:
    np.save(f, np.array(errorList))
    np.save(f, np.array(targetList))
    np.save(f, np.array(predictionList))


model = Model(input_hf=bool(sys.argv[8]),
              input_nodal_disp=bool(sys.argv[9]),
              input_fixed_nodes=bool(sys.argv[10]),
              input_mat=bool(sys.argv[11]),
              name=sys.argv[5])
db_key = sys.argv[4]

checkpoint_dir = os.path.join("dir", sys.argv[6])
dataset_dir = os.path.join("gnn_datasets", sys.argv[7])


if len(sys.argv) > 12:
  train(model=model,
        dataset_dir=dataset_dir,
        checkpoint_dir=checkpoint_dir,
        n_initial_steps=int(sys.argv[12]),
        num_training_steps=int(sys.argv[13]),
        learning_rate=float(sys.argv[14]))
else:
  eval(model=model,
       dataset_dir=dataset_dir,
       checkpoint_dir=checkpoint_dir,
       db_key=db_key)

