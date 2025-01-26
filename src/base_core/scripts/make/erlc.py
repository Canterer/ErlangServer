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
	print(cmdline)
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
		print("proc src:",src)
		fd = open(src,'r',encoding='utf-8')
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
				
				self._inc.append(incFile)
		for incFile in self._inc:
			incPath = include_path_map[incFile]
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
		print("include fileName: "+fileName+" path: "+filePath)

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
