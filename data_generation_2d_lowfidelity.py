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

class prestress_optimizer(main_tools):
    
    def __init__(self, jobName, partName, instanceName, modelName, mdb, session, viewportObj1 = 'Viewport: 1', stepName = 'EQ'):
        super(prestress_optimizer, self).__init__(jobName, partName, instanceName, modelName, mdb, session, viewportObj1)
        self.modelNameWithoutEQ = self.modelName
        self.stepName = stepName
        jobName = self.jobName
        self.odbName = jobName + '-withEQ.odb'
        self.odbNameBackup = jobName + '-withEQ-backup.odb'
        self.modelName = self.modelNameWithoutEQ + '-withEQ'
        self.jobName = jobName + '-withEQ'
        self.mdb.Model(name=self.modelName, objectToCopy=mdb.models[modelName])
        self.mdb.Job(name = self.jobName, objectToCopy = mdb.jobs[jobName])
        self.mdb.jobs[self.jobName].setValues(model=self.modelName)
        self.steps = self.mdb.models[self.modelName].steps
        self.stepsWithoutEQ = self.mdb.models[self.modelNameWithoutEQ].steps
        for stepName in self.steps.keys():
            if stepName in ['Initial', 'EQ']: continue
            self.steps[stepName].suppress()
            self.stepsWithoutEQ[stepName].suppress()
    
    def error_approximation(self, newValues):
        t = 0
        temp = []
        for i in newValues:
            t += 1
            if (t % 3 != 1): temp.append(i)
        
        return max(temp)
    
    def nodal_error(self, initialNodalCoords, newNodalCoords):
        temp = newNodalCoords
        for t in xrange(len(initialNodalCoords)):
            if (((t + 1) % 3) != 1): temp[t] = newNodalCoords[t] - initialNodalCoords[t]
        
        return temp
        
    def coords_optimizer(self, initialNodalCoords, error, jobName, odb, iterationNumber):
        maxError = 1
        while maxError > error:
            newNodalCoords = self.extract_coords_values(-1, self.stepName, odb)
            displacementFromInitial = self.nodal_error(initialNodalCoords, newNodalCoords)
            self.edit_node_by_offset(displacementFromInitial)
            self.job_submit(jobName)
            iterationNumber += 1
            maxError = self.error_approximation(displacementFromInitial)
        return iterationNumber
    
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
            self.stepsWithoutEQ[stepName].suppress()
        print('SDV optimization was successful!')


inputPath = 'input_2d.csv'
outputPath = 'output_2d_lowfidelity.csv'

iterationNumber = 100

os.chdir(r"C:\temp\HybridML")
FileName = 'NEW'
openMdb(pathName = FileName + '.cae')
mdb.saveAs(pathName = FileName + '-Backup.cae')
openMdb(pathName=FileName + '.cae')

jobName, partName, instanceName, modelName = 'PlaneStrain-lowfidelity', 'PlaneStrain', 'PlaneStrain-Hyperelastic-1', 'PlaneStrain-lowfidelity'

mt = main_tools(jobName, partName, instanceName, modelName, mdb, session) # main tools of Abaqus

modelObj = mdb.models[modelName]


np.random.seed(41)
u1List = np.random.uniform(0.2,-0.2,iterationNumber)

np.random.seed(42)
u2List = np.random.uniform(0.2,-0.2,iterationNumber)

np.random.seed(43)
timeSpanList = np.random.uniform(1.0,1000.0,iterationNumber)

np.random.seed(44)
c10List = np.random.uniform(0.1,5.0,iterationNumber)

i = 0
t = 0

for u1, u2, c10, timeSpan in zip(u1List, u2List, c10List, timeSpanList):
	i += 1
	if i % 2 == 0:
		u2 = 0.0
	else:
		u1 = 0.0
	
	
	modelObj.boundaryConditions['Comp'].setValues(u1 = u1, u2 = u2)
	# modelObj.materials['Hyperelastic'].hyperelastic.setValues(table=((c10, 0.0), ))
	modelObj.steps['UC'].setValues(timePeriod=timeSpan, initialInc=timeSpan, maxInc=timeSpan)
	
	t0 = time()
	mt.job_submit(jobName)
	t1 = time()
	with open('time_' + outputPath, 'a') as outputFile: np.savetxt(outputFile, [t1 - t0], delimiter=",")
	
	odb = mt.open_odb(mt.odbName)
	frameObjT = odb.steps['UC'].frames[-1]
	
	PLANESTRAIN_HYPERELASTIC = odb.rootAssembly.instances['PLANESTRAIN-HYPERELASTIC-1']
	
	# temp = np.array([[u1, u2 ,timeSpan, i]], dtype="float32")
	# with open(inputPath, 'a') as inputFile: np.savetxt(inputFile, temp, delimiter=",")
	
	Uarrays = frameObjT.fieldOutputs['U'].getSubset(region = PLANESTRAIN_HYPERELASTIC, position = NODAL).values
	
	temp = []
	for U in Uarrays:
		# temp.append(np.array([timeSpan, U.data[0], U.data[1], i], dtype="float32"))
		temp.append(np.array([U.data[0], U.data[1], i], dtype="float32"))
	
	temp = np.array(temp, dtype="float32")
	with open(outputPath, 'a') as outputFile: np.savetxt(outputFile, temp, delimiter=",")
	
	print i, temp.shape
	
	mt.close_odb()

