% Template makefile, used to compile and link MEX files 
function [source, ...
          headers, ...
          package, ...
          include, ...
          library, ...
          options] = mexmakefile()
        
        
    %{
    "source" is the top level c-source file, the s-function.
    
    EXAMPLE: 
    
        % Single target
        source = 'target.c'
    
        % Multiple targets
        source = {
            'target1.c'
            'target2.c'
        }    
    %}
    source = '';
    
    %{
     "headers" contains all header files that when changed should 
     trigger a rebuild
     
     EXAMPLE: 
    
        % Single target
        package = {
            'FunTemplate_interface.h'            
            'Vector.h'
        };
    
        % Multiple targets
        package = {
            { % Target 1
                'FunTemplate_interface.h'            
                'Vector.h'
            }
            { % Target 2
                'matrix.h'
            }
        };
    %}
    headers = {
    };
    
    
    %{
    "package" contains all necessary source files, except for 
    the sfunction itself.
     
	EXAMPLE: 
    
        % Single target
        package = {
            'FunTemplate.c'
            'Template2.c'
            'Template3.c'
        };
    
        % Multiple targets
        package = {
            { % Target 1
                'FunTemplate.c'
                'Template2.c'
                'Template3.c'
            }
            { % Target 2
                'doSomethingUseful.c'
            }
        };
    
    %}
    package = {
    };


    %{
    "include" represents all additional include paths.
    
     EXAMPLE:
    
        % Single target
        include = {
            'd:\SGEO\simulator\include'
            'd:\SGEO\model\include'
        };
    
        % Multiple targets
        include = {
            { % Target 1
                's:\programes\SGEO\simulator\include'
                '..\model\include'
            }
            { % Target 2
                '..\..\Library'
            }
        };
    
    %}
    include = {
    };

    
    %{
     "library" represents all libraries to be linked to the s-function.
    
     EXAMPLE: 
    
        % Single target
        library = {
            'd:\SGEO\library\rt_lib.lib'
        }; 
       
        % Multiple targets    
        library = {
            { % Target 1
                '..\Libs\winsock32.lib'
            }
            { % Target 2
                '..\Libs\libusb.so'
            }
        }
    
    
    %}    
    library = {
    };


    %{
    "options" is a list of any additional options you need for the compilation. 
    Consult "help mex" for more details. 
    
    EXAMPLE: 
        
        % Single target
        opts = {
            '-argcheck'
            '-win64' 
        }
    
        % Multiple targets
        opts = {
            { % Target 1
                '-argcheck'
                '-win64' 
            }
            { % Target 2
            } (% no options)
        }    
    %}
    options = {
    };

end
