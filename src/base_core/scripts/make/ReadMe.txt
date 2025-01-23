mmake.erl 提供基础的接口。主要通过make模块
Emakefile 文件提供  源码路径与Make、Compile选项
erl_make.erl 编译mmake.erl文件，提供cpu_core数据
make.cmd、make.py、make.sh 根据不同平台来生成Emakefile并进行编译，主要命令是escript.exe erl_make.erl
erlc.py 提供脚本操作erlc编译文件
MakeAll.cmd  提供初始参数调用erlc.py脚本进行整体全部编译

备注：
make模块提供了类似UNIX中make命令的功能，通过Emakefile文件中的规则进行编译。
escript是Erlang的脚本执行工具，允许将Erlang模块直接转换为Unix脚本执行。支持在不编译的情况下，直接从命令行运行代码。
erlc是Erlang编译器，用于将Erlang源代码编译成Beam字节码文件。
