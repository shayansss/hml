from abaqus import *
from abaqusConstants import *
import time
import os
from shutil import copyfile, rmtree
import json
import numpy as np
import sys

partName, instanceName = 'Part-1', 'Part-1-1'
analysisType = sys.argv[-1]

if analysisType == 'ood':
    iterationNumber = 10
    jobName, modelName = 'Second', 'Model-2'
else:
    iterationNumber = 30
    jobName, modelName = 'First', 'Model-1'

class main_tools(object):
    
    def __init__(self, jobName, partName, instanceName, modelName, mdb, session, viewportObj1 = 'Viewport: 1'):
        self.jobName = jobName
        self.odbName = jobName + '.odb'
        self.partName = partName
        self.modelName = modelName
        self.instanceName = instanceName
        self.mdb = mdb
        self.session = session
        self.viewportObj1 = self.session.viewports[viewportObj1]
    
    def job_submit(self, jobName):
        FirstJob = self.mdb.jobs[jobName]
        FirstJob.submit(consistencyChecking=OFF)
        FirstJob.waitForCompletion()
        return str(FirstJob.status)
    
    def open_odb(self, odbName, readOnly = True):
        try:
            return self.session.openOdb(name = odbName, readOnly = readOnly)
        except:
            import time; time.sleep(5)
            print 'open_odb() did not work.'
            return self.session.openOdb(name = odbName, readOnly = readOnly)
    
    def close_odb(self, odbName = ''):
        if odbName == '': odbName = self.odbName
        try:
            self.session.odbs[odbName].close()
        except:
            print 'close_odb did not work.'
    
    def save_and_close_odb(self, odbObj):
        odbObj.save()
        odbObj.close()
        
    def output_values(self, odb, stepName = 'Main', frameNum = -1, parameterName = 'SDV3'):
        return odb.steps[stepName].frames[frameNum].fieldOutputs[parameterName].values
    
    def edit_node_by_offset(self, offsetPars):
        self.mdb.meshEditOptions.setValues(enableUndo=True, maxUndoCacheElements=0.5)
        partObj = self.mdb.models[self.modelName].parts[self.partName]
        nodeObj = partObj.nodes
        num = 0
        for i in offsetPars:
            num += 1
            if num % 3 == 1:
                nodes = i - 1
            elif num % 3 == 2:
                u1 = i
            elif num % 3 == 0:
                u2 = i
                partObj.editNode(nodes=nodeObj[nodes], offset1=-u1, offset2=-u2, projectToGeometry=OFF)
    
    def extract_coords_values(self, frameNum = -1, stepName = 'Main', odb = ''):
        if odb == '': odb = self.open_odb(self.odbName)
        temp = []
        dispVal = self.output_values(odb, stepName, frameNum, 'COORD')
        for s in dispVal:
            temp.append(s.nodeLabel)
            temp.append(s.data[0])
            temp.append(s.data[1])
        
        return temp
    
    def integration_points_values(self, parameters = ['SDV3'], frameNum = -1, stepName = 'Main', odb = ''):
        if odb == '': odb = self.open_odb(self.odbName)
        temp = []
        for sdv in parameters:
            temp.append([])
            outputValues = self.output_values(odb, stepName, frameNum, sdv)
            for outputVal in outputValues:
                temp[-1].append(outputVal.data)
                temp[-1].append(outputVal.elementLabel)
                temp[-1].append(outputVal.integrationPoint)
        
        return temp

class prestress_optimizer(main_tools):
    
    def __init__(self, jobName, partName, instanceName, modelName, mdb, session, viewportObj1 = 'Viewport: 1', stepName = 'EQ'):
        super(prestress_optimizer, self).__init__(jobName, partName, instanceName, modelName, mdb, session, viewportObj1)
        self.modelNameWithoutEQ = self.modelName
        self.modelNameTemp = self.modelName + 'Temp'
        self.stepName = stepName
        jobName = self.jobName
        self.odbName = jobName + '-withEQ.odb'
        self.modelName = self.modelNameWithoutEQ + '-withEQ'
        self.jobNameTemp = jobName + 'Temp'
        self.jobName = jobName + '-withEQ'
        for i in [self.modelName, self.modelNameTemp]: self.mdb.Model(name=i, objectToCopy=mdb.models[modelName])
        for i in [self.jobName, self.jobNameTemp]: self.mdb.Job(name = i, objectToCopy = mdb.jobs[jobName])
        self.mdb.jobs[self.jobNameTemp].setValues(model=self.modelNameTemp)
        self.mdb.jobs[self.jobName].setValues(model=self.modelName)
        self.steps = self.mdb.models[self.modelName].steps
        self.stepsWithoutEQ = self.mdb.models[self.modelNameWithoutEQ].steps
        for stepName in self.steps.keys():
            if stepName in ['Initial', 'EQ']: continue
            self.steps[stepName].suppress()
            self.stepsWithoutEQ[stepName].suppress()
            self.mdb.models[self.modelNameTemp].steps[stepName].suppress()
    
    def error_approximation(self, newValues):
        t = 0
        temp = []
        for i in newValues:
            t += 1
            if (t % 3 != 1): temp.append(i)
        
        return max(temp)
    
    def nodal_error(self, initialNodalCoords, newNodalCoords, zeta):
        temp = newNodalCoords
        for t in xrange(len(initialNodalCoords)):
            if (((t + 1) % 3) != 1): temp[t] = (newNodalCoords[t] - initialNodalCoords[t]) * zeta
        
        return temp
    
    def new_SDV_in_fortran(self, lastSDV, initialParameters, SDVlocation = 'DATA.txt'):
        lenLastSDVi = len(lastSDV[0])
        lenLastSDV = len(lastSDV)
        lenInitialParameters = len(initialParameters)
        with open(SDVlocation, "w") as f:
            f.write('1\n')
            for j in xrange(0,lenLastSDVi,3):
                SDVlist = []
                for i in xrange(lenInitialParameters): SDVlist.append(initialParameters[i][j])
                
                for i in xrange(lenLastSDV): SDVlist.append(lastSDV[i][j])
                
                elementLabel = lastSDV[0][j+1]
                integrationPoint = lastSDV[0][j+2]
                newLine = '%s, %s' % (elementLabel, integrationPoint)
                for item in SDVlist: newLine = newLine + ', %s' % (item)
                
                f.write('%s\n' % (newLine))
            f.truncate()
    
    def finish_optimization(self):
        for stepName in self.steps.keys():
            if stepName in ['Initial', 'EQ']: continue
            
            self.steps[stepName].resume()
            self.stepsWithoutEQ[stepName].resume()
        
        del self.mdb.jobs[self.jobNameTemp]
        # del self.mdb.models[self.modelName + '-Temp']
        del self.mdb.models[self.modelNameTemp]
        # del self.mdb.models[self.modelNameTemp + '-Temp']
        print('SDV optimization was successful!')

os.chdir(r"C:\temp\HybridML")
FileName = 'NEW'

openMdb(pathName = FileName + '.cae')
mdb.saveAs(pathName = FileName + '-Backup.cae')
openMdb(pathName=FileName + '.cae')

# prameters used for pre-stressing optimization defined in the subroutine file: 
SDV = ['SDV2', 'SDV3']
newSDV = ['SDV14', 'SDV13']
initialParametersLables = ['SDV4', 'SDV5']

def run_prestress(jobName = jobName,
                  partName = partName,
                  instanceName = instanceName,
                  modelName = modelName,
                  initialParametersLables = initialParametersLables,
                  SDV = SDV,
                  newSDV = newSDV,
                  DepthFinallLocation = -1,
                  STATEV1 = 1,
                  zeta = 1.0,
                  breakPoint = 0,
                  errorLimit = 1e-4):
    '''this implements the pre-stress each time for a new material definition'''
    global mdb, session
    withoutOptimization = main_tools(jobName, partName, instanceName, modelName, mdb, session)
    SDVoptimizer = prestress_optimizer(jobName, partName, instanceName, modelName, mdb, session)
    mdb = SDVoptimizer.mdb
    partName = SDVoptimizer.partName
    instanceName = SDVoptimizer.instanceName
    stepName = SDVoptimizer.stepName
    odbName = SDVoptimizer.odbName
    jobName = SDVoptimizer.jobName
    modelName = SDVoptimizer.modelName
    close_odb = SDVoptimizer.close_odb
    job_submit = SDVoptimizer.job_submit
    edit_node_by_offset = SDVoptimizer.edit_node_by_offset
    finish_optimization = SDVoptimizer.finish_optimization
    extract_coords_values = SDVoptimizer.extract_coords_values
    nodal_error = SDVoptimizer.nodal_error
    error_approximation = SDVoptimizer.error_approximation
    integration_points_values = SDVoptimizer.integration_points_values
    new_SDV_in_fortran = SDVoptimizer.new_SDV_in_fortran
    open_odb = SDVoptimizer.open_odb
    odbNameWithoutOptimizaion = withoutOptimization.odbName
    jobNameWithoutOptimizaion = withoutOptimization.jobName
    
    initialTXT = '0\n%s, %s' % (DepthFinallLocation, STATEV1)

    def reset_data_txt():
        with open('DATA.txt', "w") as f: f.write(initialTXT)
    
    reset_data_txt()
    
    if job_submit(jobNameWithoutOptimizaion) == 'ABORTED': raise Exception('ERROR! TOTALLY UNSTABLE MODEL')
    
    odbObjWithoutOptimization = open_odb(odbNameWithoutOptimizaion)
    initialNodalCoords = extract_coords_values(0, stepName, odbObjWithoutOptimization)
    refSDV = integration_points_values(SDV, 0, stepName, odbObjWithoutOptimization)
    initialParameters = integration_points_values(initialParametersLables, 0, stepName, odbObjWithoutOptimization)
    lenSDVi = len(refSDV[0])
    lenSDV = len(SDV)
    copyfile(odbNameWithoutOptimizaion, odbName)
    
    iterationNumber = 0
    while True:
        if iterationNumber > 50: raise Exception('ERROR! HIGH ITERATION NUMBER')
        
        maxError = 0
        if job_submit(jobName) != 'ABORTED':
            iterationNumber += 1
            newNodalCoords = extract_coords_values(-1, stepName)
            displacementFromInitial = nodal_error(initialNodalCoords, newNodalCoords, zeta)
            
            odb = open_odb(odbName)
            firstSDV = integration_points_values(SDV, 0, stepName, odb)
            lastSDV = integration_points_values(newSDV, -1, stepName, odb)
            for i in [0 , 1]:
                for j in xrange(0,lenSDVi,3):
                    errorij = abs(lastSDV[i][j]-refSDV[i][j])/refSDV[i][j]
                    if maxError < errorij: maxError = errorij
            
            maxErrorCoords = error_approximation(displacementFromInitial)
            print '** SDV ERROR: %s, ITERATION NUMBER: %s, COORD ERROR: %s **' % (
                maxError, iterationNumber, maxErrorCoords)
            if errorLimit > maxError and 1e-3 > maxErrorCoords: break
            
            if zeta < 0.5 and iterationNumber - breakPoint > 5: zeta = zeta*2
            
            # updating the material changes due to the deformation gradient tensor
            for i in xrange(lenSDV): 
                for j in xrange(0,lenSDVi,3):
                    if i < 2:
                        lastSDV[i][j] = (firstSDV[i][j] - zeta*(lastSDV[i][j] - refSDV[i][j]))
            
            # Updating the coordination changes
            edit_node_by_offset(displacementFromInitial)
            close_odb(odbName)
            copyfile('DATA.txt', 'DATAbackup.txt')
            new_SDV_in_fortran(lastSDV, initialParameters)
        else:
            zeta = zeta/2
            if zeta < 0.1: raise Exception('ERROR! ZETA < 0.1')
            
            close_odb(odbName)
            copyfile('DATAbackup.txt', 'DATA.txt')
            if breakPoint != iterationNumber:
                del mdb.models[modelName]
            
            breakPoint = iterationNumber
    
    finish_optimization()
    return SDVoptimizer, reset_data_txt


np.random.seed(41+iterationNumber)
u1List = np.random.uniform(0.1,-0.1,iterationNumber)

np.random.seed(42+iterationNumber)
u2List = np.random.uniform(0.1,-0.1,iterationNumber)

np.random.seed(43+iterationNumber)
alpha1List = np.random.uniform(0.005,0.010,iterationNumber)

np.random.seed(44+iterationNumber)
timeSpanList = np.random.uniform(1.0,1000.0,iterationNumber)


writePath = os.path.join("gnn_datasets", modelName)

def new_address(filePath, version = None):
    return os.path.join(writePath, filePath)

if os.path.exists(writePath): rmtree(writePath)

os.makedirs(writePath)

meta_features = {}

def store_files(key, array, shape):
	# save array and update meta_features.
	if shape[0] == 1:
		frame_type = "static"
	else:
		frame_type = "dynamic"
	
	meta_features[key] = {"type": frame_type,
						  "shape": shape,
						  "dtype": array.dtype.name}
	
	with open(new_address(key) + ".csv", "a") as f:
		np.savetxt(f, array.reshape(-1), delimiter=",")


sample_num = 0
for u1, u2, alpha1, timeSpan in zip(u1List, u2List, alpha1List, timeSpanList):
    for prestress in [True, False]:
        t0 = time.time()
        
        if prestress == False:
            mt = main_tools(jobName, partName, instanceName, modelName, mdb, session)
            time_csv_name = 'time_lf.csv'
        else:
            mt, reset_data_txt = run_prestress()
            time_csv_name = 'time_hf.csv'
        
        
        modelObj = mdb.models[mt.modelName]
        modelObj.boundaryConditions['disp'].setValues(u1 = u1, u2 = u2)
        
        # it is neccassary to start without pre-stressing to set alpha1 for both,
        # otherwise, prestressing runs without the correct alpha1 value.
        modelObj.materials['DZ'].userMaterial.setValues(mechanicalConstants=(0.0, 0.0, alpha1))
        
        
        # our noise generator that addes the noise first on the high-fidelity model,
        # and then keeps it for the low-fidelity model.
        if prestress == True and mt.modelName[:7] == "Model-1": 
            if sample_num != 0:
                del mt.mdb.models[mt.modelName]
                mt.mdb.Model(name=mt.modelName, objectToCopy=mt.mdb.models[mt.modelName+'-withoutOffsets'])
            else:
                mt.mdb.Model(name=mt.modelName+'-withoutOffsets', objectToCopy=modelObj)
            
            nodalCoord = mt.extract_coords_values(frameNum = 0, stepName = 'EQ')
            np.random.seed(sample_num)
            if modelName == 'Model-1':
                randList = np.random.rand(len(nodalCoord))/300
            else:
                randList = np.random.rand(len(nodalCoord))/200
            
            temp = nodalCoord[:]
            for t in xrange(len(nodalCoord)):
                if (((t + 1) % 3) != 1): temp[t] = randList[t]
            
            mt.edit_node_by_offset(temp)
        
        mt.job_submit(mt.jobName)
        t1 = time.time()
        
        with open(new_address(time_csv_name), 'a') as f: np.savetxt(f, [t1 - t0], delimiter=",")
        
        odb = mt.open_odb(mt.odbName)
        frames = odb.steps['DISP'].frames
        assemblyObj = odb.rootAssembly
        instanceObj = assemblyObj.instances[mt.instanceName.upper()]
        
        if prestress == True:
            def extract_node_labels(nodeSetName):
                node_data = frames[0].fieldOutputs['COORD'].getSubset(region=assemblyObj.nodeSets[nodeSetName],
                                                                      position=NODAL).values
                return np.array([d.nodeLabel for d in node_data]).reshape(-1)
            
            node_labels_all = extract_node_labels(' ALL NODES')
            num_nodes = node_labels_all.shape[0]
            
            node_type_fixed = extract_node_labels('FIXED')
            mask = np.isin(node_labels_all, node_type_fixed)
            node_type_fixed = np.where(mask,1,0)
            store_files("fixed_nodes", node_type_fixed, [1, num_nodes, 1])
            
            node_type_disp  = extract_node_labels('DISP')
            mask = np.isin(node_labels_all, node_type_disp)
            node_type_disp = np.zeros((node_labels_all.shape[0], 2))
            node_type_disp[mask] = np.array([u1, u2])
            store_files("nodal_disp", node_type_disp, [1, num_nodes, 2])
            
            mat = np.array(num_nodes*[alpha1], dtype="float32") # for all nodes
            store_files("mat", mat, [1, num_nodes, 1])
            
            num_elements = np.array([element.label for element in instanceObj.elements]).shape[0]
            cells = np.array([element.connectivity for element in instanceObj.elements])
            store_files("cells", cells, [1,num_elements,3])
            
            temp = frames[0].fieldOutputs['COORD'].getSubset(region = instanceObj, position = NODAL).values
            world_pos = np.array([step_values.data for step_values in temp], dtype="float32")
            store_files("world_pos", world_pos, [1, num_nodes, 2])
            
            temp = frames[-1].fieldOutputs['U'].getSubset(region = instanceObj, position = NODAL).values
            hf_deformation = np.array([step_values.data for step_values in temp], dtype="float32")
            temp = frames[-1].fieldOutputs['S'].getSubset(region = instanceObj, position = NODAL).values
            hf_stress = np.array([step_values.data for step_values in temp], dtype="float32")
            hf_res = np.concatenate((hf_deformation, hf_stress), axis=-1)
            store_files("hf_res", hf_res, [1, num_nodes, hf_res.shape[-1]])
            
        else:
            temp = frames[-1].fieldOutputs['U'].getSubset(region = instanceObj, position = NODAL).values
            lf_deformation = np.array([step_values.data for step_values in temp], dtype="float32")
            temp = frames[-1].fieldOutputs['S'].getSubset(region = instanceObj, position = NODAL).values
            lf_stress = np.array([step_values.data for step_values in temp], dtype="float32")
            lf_res = np.concatenate((lf_deformation, lf_stress), axis=-1)
            store_files("lf_res", lf_res, [1, num_nodes, lf_res.shape[-1]])
            
            reset_data_txt()
        
        print "Sample num: %s (prestress: %s)"%(sample_num + 1, prestress)
        
        mt.close_odb()
    
    sample_num += 1


meta = {}
meta["features"] = meta_features
meta["trajectory_length"] = 1
meta["field_names"] = meta_features.keys()
meta["total_num_samples"] = sample_num
meta["collision_radius"] = None

with open(new_address("meta.json"), "w") as f: json.dump(meta, f, indent=2, separators=(", ", ": "))
