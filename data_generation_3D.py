from abaqus import *
from abaqusConstants import *
import section
import regionToolset
import displayGroupMdbToolset as dgm
import part
import material
import assembly
import step
import interaction
import load
import mesh
import optimization
import job
import sketch
import visualization
import xyPlot
import displayGroupOdbToolset as dgo
import connectorBehavior
import time
import os
from shutil import copyfile
import numpy as np
from time import time

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
    
    def edit_node_by_offset(self ,offsetPars):
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
            temp.append(s.dataDouble[0])
            temp.append(s.dataDouble[1])
        
        return temp
        
    def extract_nodal_values(self, frameNum = -1, stepName = 'Main', odb = ''):
        if odb == '': odb = self.open_odb(self.odbName)
        temp = []
        dispVal = self.output_values(odb, stepName, frameNum, 'COORD')
        for s in dispVal:
            temp.append(s.nodeLabel)
            temp.append(s.dataDouble[0])
            temp.append(s.dataDouble[1])
        
        return temp
    
    def integration_points_values(self, parameters = ['LE22'], frameNum = -1, stepName = 'UC', odb = ''):
        if odb == '': odb = self.open_odb(self.odbName)
        temp = []
        for sdv in parameters:
            temp.append([])
            outputValues = self.output_values(odb, stepName, frameNum, sdv)
            for outputVal in outputValues:
                temp[-1].append([outputVal.data, outputVal.elementLabel, outputVal.integrationPoint])
        
        return temp

os.chdir(r"C:\temp\HybridML")
FileName = 'Open_knee_native'
openMdb(pathName = FileName + '.cae')
mdb.saveAs(pathName = FileName + '-Backup.cae')
openMdb(pathName=FileName + '.cae')

# jobName, partName, instanceName, modelName = 'Job-1-withEQ', 'femur_cartilage', 'femur_cartilage-1', 'OpenKnee_model-withEQ'
jobName, partName, instanceName, modelName = 'Job-1', 'femur_cartilage', 'femur_cartilage-1', 'OpenKnee_model'
# jobName, partName, instanceName, modelName = 'Job-1-mono', 'femur_cartilage', 'femur_cartilage-1', 'OpenKnee_model-mono'
modelObj = mdb.models[modelName]
mt = main_tools(jobName, partName, instanceName, modelName, mdb, session) # main tools of Abaqus


iterationNumber = 100
inputPath = 'input_3d.csv'
outputPath = 'output_3d_highfidelity.csv'

for i in [inputPath, outputPath]: open(i, 'w').close()

np.random.seed(14)
cf2List = np.random.uniform(-900.0,-400.0,iterationNumber)
# np.random.seed(15)
# timeSpanList = np.random.uniform(0.1,4.0,iterationNumber)
# Clist = np.random.uniform(1.0,2.0,iterationNumber)

timeSpan = 10

t = 0
i = 0
for cf2 in (cf2List):
    i += 1
    # if i <= 35: continue
    
    # modelObj.materials['CAR'].hyperelastic.setValues(table=((C), ))
    
    modelObj.loads['Static_Load'].setValues(cf2=cf2)
    
    modelObj.steps['LOAD'].setValues(timePeriod=timeSpan,
                                     initialInc=timeSpan/2,
                                     maxInc=timeSpan)
    
    t0 = time()
    mt.job_submit(jobName)
    t1 = time()
    with open('time_' + inputPath, 'a') as outputFile: np.savetxt(outputFile, [t1 - t0], delimiter=",")
    
    odb = mt.open_odb(mt.odbName)
    frameObjT = odb.steps['LOAD'].frames[-1]
    # temp = np.array([[cf2 ,timeSpan]], dtype="float32")
	
	
    # temp = np.array([[cf2, C]], dtype="float32")
    temp = np.array([[cf2]], dtype="float32")
    with open(inputPath, 'a') as inputFile: np.savetxt(inputFile, temp, delimiter=",")
    
    region = odb.rootAssembly.instances['PART-1-1'].elementSets['TIBIA_CARTILAGE_MED-1__PICKEDSET26']
    # GAGarrays = frameObjT.fieldOutputs['SDV6'].getSubset(region = region, position = ELEMENT_NODAL).values
    Sarrays   = frameObjT.fieldOutputs['S'].getSubset(region = region, position = ELEMENT_NODAL).values
    LEarrays  = frameObjT.fieldOutputs['LE'].getSubset(region = region, position = ELEMENT_NODAL).values
    
    temp = []
    for (S, LE) in zip(Sarrays, LEarrays):
        temp.append(np.array([S.data[0],
                              S.data[1],
                              S.data[2],
                              LE.data[0],
                              LE.data[1],
                              LE.data[2]], dtype="float32"))
    
    temp = np.array(temp, dtype="float32")
    with open(outputPath, 'a') as outputFile: np.savetxt(outputFile, temp, delimiter=",")
    
    
    
    # region = odb.rootAssembly.instances['PART-1-1'].nodeSets['TIBIA_CARTILAGE_MED-1__PICKEDSET26']
    # Uarrays = frameObjT.fieldOutputs['U'].getSubset(region = region, position = NODAL).values
    # temp = []
    # for U in Uarrays:
        # temp.append(np.array(U.data[0:3], dtype="float32"))
    
    # temp = np.array(temp, dtype="float32")
    # with open(outputPath, 'a') as inputFile: np.savetxt(inputFile, temp, delimiter=",")
    
    print i, temp.shape
    
    mt.close_odb()

