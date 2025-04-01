#!/usr/bin/env python
import os
import sys


# cwd = os.path.abspath(os.path.split(sys.argv[0])[0])
# os.chdir(cwd)
print("cwd:"+os.getcwd())

class Options(object):
	def proc_argv(self,argv):
		i = 1;
		argc = len(argv)
		opt = ''
		opt_params= []
		config = self._config
		while(i<argc):
			if str.find(argv[i],'--')!=-1:
				newopt = str.replace(argv[i],'-','',1)
				if opt !='':
					config[opt] = opt_params
				opt = newopt
				opt_params = []
			else:
				opt_params.append(argv[i])
			i=i+1
		if opt !='':
			config[opt] = opt_params
			
	def __init__(self,argv):
		self._argv = argv
		self._config = {}
		Options.proc_argv(self,argv)

	def get_value(self,optname):
		try:
			value = self._config[optname]
		except:
			value = ['']
		return value
			
def erl_compile(debug,srcfile,inc_dir_map,hipe,out_dir,nowarning_opt,defines):
	if sys.platform=='linux2':
		cmdline_header = 'erlc -W '
	elif sys.platform=='darwin':
		cmdline_header = 'erlc -W '
	else:
		cmdline_header = 'erlc.exe -W '
	Inc = ''
	for inc_dir in inc_dir_map:
		Inc = Inc + ' -I ' + inc_dir +' '

	if debug =='true':
		Dbg = ' +debug_info '
	else:
		Dbg = ''

	if hipe =='true':
		Hipe = ' +native +"{hipe, [o3]}" '
	else:
		Hipe = ''

	if srcfile !='':
		Src = srcfile
		
	if out_dir !='':
		Out = ' -o '+ out_dir

	if nowarning_opt == 'true':
		NoWarning = ' +nowarn_unused_vars +nowarn_unused_function '
	else:
		NoWarning = ''

	Defines = ''
	if len(defines) !=0:
		for DM in defines:
			if DM !='':
				Defines+= ' -D'+DM
	else:
		Defines = ''
		
	cmdline = cmdline_header + Out + Inc + Dbg  + Defines + Hipe + NoWarning + Src
	# print(cmdline)
	os.system(cmdline)

opt = Options(sys.argv)

# include_path = opt.get_value('-I')[0]
dbg = opt.get_value('-debug')[0]
hipe = opt.get_value('-hipe')[0]
out_dir = opt.get_value('-output')[0]
src_dir = opt.get_value('-src')[0]
nowarning = opt.get_value('-nowarning')[0]
defines=opt.get_value('-define')

print('--define ',defines)
print('--src '+src_dir)
src_dir = os.path.join(os.getcwd(),src_dir)
src_dir = os.path.abspath(src_dir)
print('src_dir:'+src_dir)
print('--output '+out_dir)
out_dir = os.path.join(os.getcwd(),out_dir)
out_dir = os.path.abspath(out_dir)
if not os.path.exists(out_dir):
	os.makedirs(out_dir)

print('out_dir:'+out_dir)


class erl_object(object):
	def __init__(self,src):
		self._src = src
		self._inc = []
		self._inc_path_map = {}
		erl_object.proc_erl(self,src)

	def proc_erl(self,src):
		# print("proc src:",src)
		fd = open(src,'r',encoding='utf-8')
		lines = fd.readlines()
		fd.close()
		incstr = '-include("'
		incstr2= '").'
		recordMap = {}
		for line in lines:
			if line[0]!='%' and line.find(incstr) != -1:
				line = line.replace(incstr,'')
				line = line.replace('\n','')
				line = line.replace('\r','')
				line = line.replace('\t','')
				line = line.replace(' ','')
				incFile = line.replace(incstr2,'')
				
				if not recordMap.get(incFile, False):
					recordMap[incFile] = True
					self._inc.append(incFile)
					subFileList = include_sub_list_map[incFile]
					for subFile in subFileList:
						if recordMap.get(subFile, False):
							recordMap[subFile] = True
							self._inc.append(subFile)
		for incFile in self._inc:
			incPaths = include_dir_map[incFile]
			for incPath in incPaths:
				if not self._inc_path_map.get(incPath, False):
					self._inc_path_map[incPath] = True

				
	def check_modified(self,out):
		# print('now checking '+self._src)
		path = os.path.split(self._src)
		path1 = os.path.splitext(path[1])
		dstfile = out +'/' + path1[0] + '.beam'
		srcfile = self._src
		
		if not os.path.exists(os.path.abspath(dstfile)) :
			print('not exists:' + dstfile)
			return True
		if os.stat(os.path.abspath(dstfile)).st_mtime < os.stat(os.path.abspath(srcfile)).st_mtime:
			print('timeout:' + dstfile)
			return True
		for incFile in self._inc:
			incFilePath = os.path.join(include_path_map[incFile],incFile)  
			print("incFile path:"+incFilePath)
			if os.path.exists(os.path.abspath(incFilePath)) :
				if os.stat(os.path.abspath(dstfile)).st_mtime < os.stat(os.path.abspath(incFilePath)).st_mtime:
					print('include timeout:'+dstfile)
					return True
				else:
					return False
			else:
				print('not exists include:'+ incFilePath +'  outfile:'+ dstfile)
				return True
		return False
	
		
	def erlc(self,debug,hipe,out_dir,nowarning_opt,defines):
		if erl_object.check_modified(self,out_dir):
			erl_compile(dbg,self._src,self._inc_path_map,hipe,out_dir,nowarning_opt,defines)
		

def scan_dir(files,input_dir,suffix):
	for file in os.listdir(input_dir):
		if file =='.svn':
			continue
		absfile = os.path.join(input_dir,file)
		if os.path.isdir(absfile):
			scan_dir(files,absfile,suffix)
		else:
			path = os.path.splitext(absfile)
			if len(path) == 2 and path[1] == suffix:
				files.append(absfile)


include_path_map = {}
include_dir_map = {}# hrl 的include目录列表
include_sub_list_map = {}# hrl 引入的其他hrl列表

# 计算某个hrl 需要引入的目录列表(包含自身以及引入其他的  路径目录)
def cacl_include_paths(fileName,filePath,subFileList):
	paths = include_dir_map.get(fileName, [])
	if len(paths) > 0:
		return paths

	tempList = [filePath]
	for subFileName in subFileList:
		subFilePath = include_path_map.get(subFileName)
		ssubList = include_sub_list_map.get(subFileName)
		tempList.extend(cacl_include_paths(subFileName,subFilePath,ssubList))
	# 路径去重
	pathMap = {}
	pathList = []
	for path in tempList:
		bRepeat = pathMap.get(path, False)
		if not bRepeat:
			pathMap[path] = True
			pathList.append(path)
	include_dir_map[fileName]=pathList
	# print("include file:{0} subFileList:{1} \ndir_map:{2}".format(fileName,subFileList,pathList))
	return pathList

# 扫描include依赖文件
files = []
scan_dir(files,src_dir,'.hrl')  
for filePath in files:
	existFilePath = include_path_map.get(filePath, False)
	if existFilePath:
		print("Error same fileName!!!\n")
		print(filePath+"\n"+existFilePath)
	else:
		path = os.path.split(filePath)
		fileName = path[1]
		include_path_map[fileName]=path[0]
		# print("include fileName: "+fileName+" path: "+filePath)

		# 递归引用
		incList = []
		fd = open(filePath,'r',encoding='utf-8')
		lines = fd.readlines()
		fd.close()
		incstr = '-include("'
		incstr2= '").'
		for line in lines:
			if line[0]!='%' and line.find(incstr) != -1:
				line = line.replace(incstr,'')
				line = line.replace('\n','')
				line = line.replace('\r','')
				line = line.replace('\t','')
				line = line.replace(' ','')
				incFile = line.replace(incstr2,'')
				incList.append(incFile)
		include_sub_list_map[fileName] = incList
		# print("include fileList:{0}".format(incList))

# 计算每个hrl的 引入目录列表
for fileName, path in include_path_map.items():
	subFileList = include_sub_list_map.get(fileName, [])
	cacl_include_paths(fileName, path, subFileList)
	

# 扫描app.src文件
files = []
scan_dir(files,src_dir,'.src')  
for filePath in files:
	path = os.path.split(filePath)
	name = os.path.splitext(path[1])[0]
	splitList = os.path.splitext(name)
	if len(splitList) == 2 and splitList[1] == '.app':
		print("find "+name+" path: "+filePath)
		copyFilePath = os.path.join(out_dir, name)
		with open(copyFilePath, 'wb') as copyFile:
			with open(filePath, 'rb') as file:
				content = file.read()
				copyFile.write(content)
	else:
		print("error .src suffix file: "+filePath)


files = []
scan_dir(files,src_dir,'.erl')	
for item in files:
	obj = erl_object(item)
	obj.erlc(dbg,hipe,out_dir,nowarning,defines)
