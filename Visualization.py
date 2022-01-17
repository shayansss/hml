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
# mdb.saveAs(pathName = FileName + '-Backup.cae')

jobName, partName, instanceName, modelName = 'Job-1', 'femur_cartilage', 'femur_cartilage-1', 'OpenKnee_model'
modelObj = mdb.models[modelName]
mt = main_tools(jobName, partName, instanceName, modelName, mdb, session) # main tools of Abaqus
odb = mt.open_odb(mt.odbName, readOnly = False)

viewportObj = session.viewports['Viewport: 1']
viewportObj.setValues(displayedObject=odb)
viewportObj.makeCurrent()


viewportObj.viewportAnnotationOptions.setValues(triad=OFF, legend=OFF, title=OFF, state=OFF, annotations=OFF, compass=OFF)

# START OF AUTOMATICALLY GENERATED PYTHON CODE
path = "temp_temp"
# END OF AUTOMATICALLY GENERATED PYTHON CODE

data = np.loadtxt(path, delimiter=",", dtype='float32').reshape(-1,1)

odbInstance = odb.rootAssembly.instances['PART-1-1']
region = odbInstance.nodeSets['TIBIA_CARTILAGE_MED-1__PICKEDSET26']
Sarrays   = odb.steps['LOAD'].frames[-1].fieldOutputs['S'].getSubset(region = region, position = ELEMENT_NODAL).values
region = [i.nodeLabel for i in Sarrays]

name = 'temp_%s'%(np.random.random())

newFieldOutput = odb.steps['LOAD'].frames[0].FieldOutput(name=name, description='', type=SCALAR)
newFieldOutput.addData(position=NODAL, instance=odbInstance, labels=region, data=data)
mt.save_and_close_odb(odb)
odb = mt.open_odb(mt.odbName)

viewportObj = session.viewports['Viewport: 1']
viewportObj.setValues(displayedObject=odb)

viewportObj.odbDisplay.display.setValues(plotState=(CONTOURS_ON_DEF, ))
leaf = dgo.LeafFromElementSets(elementSets=('PART-1-1.TIBIA_CARTILAGE_MED-1__PICKEDSET26', ))
viewportObj.odbDisplay.displayGroup.replace(leaf=leaf)
viewportObj.view.setValues(cameraPosition=(85.8183, 423.226, 45.6801), cameraUpVector=(0, 0, 1))
viewportObj.odbDisplay.contourOptions.setValues(maxAutoCompute=OFF, maxValue=2, minAutoCompute=OFF, minValue=0)
viewportObj.view.fitView()
viewportObj.odbDisplay.contourOptions.setValues(outsideLimitsMode=SPECTRUM)


viewportObj.odbDisplay.setFrame(step=0, frame=0)
viewportObj.setValues(displayedObject=odb)
viewportObj.odbDisplay.setPrimaryVariable(variableLabel=name, outputPosition=NODAL)

session.pngOptions.setValues(imageSize=(4000, 2536))
session.printOptions.setValues(reduceColors=False)
session.printToFile(fileName='%s'%(path), format=PNG, canvasObjects=(viewportObj, ))


