% Compilation function ====================================================

% This function allows for simple and generic makefiles. Moreover,
% it handles and displays errors much more smoothly than a bare
% call to MEX.

function mexmake(varargin)

    assert(nargout <= 2,...
           [mfilename() ':invalid_argument_count'],...
           'Too many output arguments.');

    [options, makefiles, filters] = parse_arguments(varargin{:});

    num_makefiles = numel(makefiles);

    if num_makefiles > 0

        for ii = 1:num_makefiles
            process_makefile(makefiles{ii}, filters, options); end

    elseif nargout==0
        disp('No MEX-makefiles found under the current directory.');
    end

end


% Parse command line arguments / function inputs
function [options,...
          makefiles,...
          filters] = parse_arguments(varargin)

    % handy-dandy abreviation
    uf = {'UniformOutput', false};

    % Parse function inputs / command line arguments
    moreopts  = true;
    filters   = {};
    makefiles = {};
    dirs      = {};
    options   = struct('target_path'  , '',...    % empty means don't move it
                       'force_rebuild', false, ...
                       'display'      , true,  ...
                       'debug'        , false, ...
                       'profiling'    , false, ...
                       'architecture' , computer('arch'),...
                       %{
                       Variables for internal use
                       %}
                       'is_MS', ... % As usual, we have to make some exceptions for MS
                           any(strcmpi({mex.getCompilerConfigurations().Manufacturer}, 'Microsoft'))...
                       );

    while ~isempty(varargin)

        parameter = varargin{1};

        if moreopts && ischar(parameter) && parameter(1)=='-'
            switch lower(parameter)

                % standalone parameters
                case '--'
                    moreopts = false;
                    continue;

                case {'-f' '--force'}
                    options.force_rebuild = true;
                    varargin(1) = [];
                    continue;

                case {'-q' '--quiet'}
                    options.display = false;
                    varargin(1) = [];
                    continue;

                case {'-g' '-d' '--debug'}
                    options.debug = true;
                    varargin(1) = [];
                    continue;

                case {'-p' '--profiling'}
                    options.profiling = true;
                    varargin(1) = [];
                    continue;

                % parameters with arguments
                case {'-a' '--architecture' '--arch'}

                    if numel(varargin) > 1
                        arch = varargin{2};
                        varargin(1:2) = [];
                    else
                        error([mfilename() ':invalid_usage'],...
                              ['Parameter ''--architecture'' must be followed by a valid ',...
                              'architecture supported by mex().']);
                    end

                    assert(ischar(arch), [...
                           'Value for parameter ''--architecture'' must be ',...
                           'a string; got ''%s''.'],...
                           class(targetPath));

                    assert(any(strcmpi(arch,{'win32' 'win64' 'glnx86' 'glnxa64' 'maci' 'maci64'})), ...
                        ['Invalid architecture string found: ''%s''. See ''doc computer'' for a list ',...
                        'of valid architecture strings.']);

                    options.architecture = arch;
                    continue;

                case {'-t' '--targetdir'}

                    if numel(varargin) > 1
                        targetPath = varargin{2};
                        varargin(1:2) = [];
                    else
                        error([mfilename() ':invalid_usage'],...
                            ['Parameter ''--targetdir'' must be followed by a valid ',...
                            'directory name.']);
                    end

                    assert(ischar(targetPath), ...
                        'Value for parameter ''--targetdir'' must be a string; got ''%s''.',...
                        class(targetPath));

                    if exist(targetPath,'dir')~=7
                        warning([mfilename() ':targetdir_not_found'],...
                            'Target directory ''%s'' is not a valid directory.', targetPath);
                        continue;
                    end

                    options.target_path = cd(cd(targetPath));
                    continue;

                otherwise
                    warning(...
                        [mfilename() ':unsupported_parameter'],...
                        'Unsupported parameter: ''%s''; ignoring...', parameter);
                    varargin(1) = [];
                    continue;
            end
        end

        % Dirname or path to a makefile, or a specific target
        if ischar(parameter)

            varargin(1) = [];
            [~, filename, extension] = fileparts(parameter);

            % Path to a makefile
            if exist(parameter,'file')==2
                if strcmp(filename,'mexmakefile') && strcmp(extension, '.m')
                    makefiles{end+1} = parameter; %#ok<AGROW>
                else
                    warning([mfilename() ':makefile_not_found'], [...
                            'MEX-makefile ''%s'' could not be found, or ',...
                            'does not appear to be a valid MEX-makefile. ',...
                            'Ignoring...'], parameter);
                    continue;
                end

            % Dirname of a path containing a makefile
            elseif exist(parameter,'dir')==7
                dirs{end+1} = parameter; %#ok<AGROW>

            % Everything else goes to filter list
            else
                filters{end+1} = parameter; %#ok<AGROW>

            end

        else
            warning([mfilename() ':unsupported_parameter'],...
                    'Got parameter of unsupported class ''%s''. Ignoring...',...
                    class(parameter));
            continue;
        end

    end

    % If no makefiles and no dirs have been given, search the
    % current dir recursively
    if isempty(dirs) && isempty(makefiles)
        dirs = {'.'}; end

    % If we have any dirs, search them recursively for MEX-makefiles
    if ~isempty(dirs)
        prevDir = pwd;
        for ii = 1:numel(dirs)
            try
                cd(dirs{ii});
                additional_makefiles = Utils.System.RecursiveDirlist('mexmakefile.m');
                cd(prevDir);
                if ~isempty(additional_makefiles)
                    additional_makefiles = cellfun(...
                        @fileparts, {additional_makefiles.name}', uf{:});
                else
                    continue;
                end
            catch ME
                cd(prevDir);
                warning([mfilename() ':recursive_search_failed'], [...
                        'Something went wrong while locating MEX-makefiles ',...
                        'in directory ''%s'':'],...
                    dirs{ii});
                warning(ME.identifier, '%s', ME.message);
                continue;
            end
            makefiles = [makefiles; additional_makefiles];  %#ok<AGROW>
        end
    end

end


% Pre-process MEX-makefile and call the build function
% on all sources it contains
function process_makefile(buildPath,...
                          filters,...
                          flags)

    prevDir = pwd;
    OC = onCleanup(@(~) cd(prevDir));

    % Skip the template makefile
    if isequal(buildPath, fileparts(mfilename('fullpath')))
        return; end

    % To avoid most pathing conflicts, just go to the build path
    cd(buildPath);

    % The make file may not exist
    assert(exist(fullfile('.','mexmakefile.m'),'file')==2,...
           'MEX-makefile not found.');

    % NOTE: (Rody Oldenhuis) we might be inside a package/class directory;
    % see
    % https://stackoverflow.com/questions/52863129/
    % https://stackoverflow.com/questions/53057584/ (and related)
    % for why we have to use this weird construction:
    fqn = @Utils.Matlab.FunctionCallFromPath;
    mexmakefile_caller = str2func(fqn(fullfile(pwd(), 'mexmakefile.m')));

    % Now, evaluate makefile to get all parameters
    [sources,...
     headers,...
     packages,...
     includes,...
     libraries,...
     options] = mexmakefile_caller();

    % Basic checks
    if isempty(sources)
        warning([mfilename() ':no_source_files'],...
                'No source files specified; nothing to do.');
        return;
    end

    assert(   iscell(headers)   ...
           && iscell(packages)  ...
           && iscell(includes)  ...
           && iscell(libraries) ...
           && iscell(options),...
           'A makefile''s output arguments should all be cell-arrays.');

    % There can be a single source file (string) or multiple ones (cellstring)
    if ~iscell(sources)

        % Basic checks
        assert(ischar(sources), ...
               [mfilename() ':invalid_argument'],...
               'Source file name should be a string.');
        assert(   iscellstr(packages)  ...
               && iscellstr(headers)   ...
               && iscellstr(includes)  ...
               && iscellstr(libraries) ...
               && iscellstr(options), ...
               [mfilename() ':invalid_argument'], [...
               'A makefile''s output arguments should all be cell-arrays ',...
               'containing strings.']); %#ok<ISCLSTR>

        % Cast everything into a single-element cell
        sources   = {sources};
        headers   = {headers};
        packages  = {packages};
        includes  = {includes};
        libraries = {libraries};
        options   = {options};

    else
        % Basic checks
        assert(all(cellfun('isclass', headers,   'cell')) && all(cellfun(@iscellstr, headers  )) && ...
               all(cellfun('isclass', packages,  'cell')) && all(cellfun(@iscellstr, packages )) && ...
               all(cellfun('isclass', includes,  'cell')) && all(cellfun(@iscellstr, includes )) && ...
               all(cellfun('isclass', libraries, 'cell')) && all(cellfun(@iscellstr, libraries)) && ...
               all(cellfun('isclass', options,   'cell')) && all(cellfun(@iscellstr, options  )),[...
               'For multiple sources, a makefile''s output arguments ',...
               'must all be cell-arrays containing cellstrings.']);

        % We should of course still allow empties
        if isempty(headers)  , headers   = repmat({{}}, numel(sources),1); end
        if isempty(packages) , packages  = repmat({{}}, numel(sources),1); end
        if isempty(includes) , includes  = repmat({{}}, numel(sources),1); end
        if isempty(libraries), libraries = repmat({{}}, numel(sources),1); end
        if isempty(options)  , options   = repmat({{}}, numel(sources),1); end

        assert(isequal(numel(sources),...
                       numel(headers),...
                       numel(packages),...
                       numel(includes),...
                       numel(libraries),...
                       numel(options)), ...
               'Inconsistent dimensions of makefile''s output argmuents.');
    end

    % Now process all the individual sources
    if ~isempty(sources)

        skips = false(size(sources));

        % Apply filter if any
        if ~isempty(filters)
            for ii = 1:numel(sources)
                if all(cellfun('isempty', regexpi(sources{ii}, filters)))
                    skips(ii) = true; end
            end
        end

        built      = cell(numel(sources));
        T          = Tasking.Task;
        T.display  = flags.display;
        T.callback = @build_mexfile;
        T.handler  = @finalize;

        for ii = 1:numel(sources)

            if skips(ii)
                continue; end

            [~,sourceName] = fileparts(sources{ii});

            mexOpt = mexext('all');
            ext = {mexOpt.ext};
            ext = ext{strcmpi(flags.architecture, {mexOpt.arch})};

            T.message = ['Building ' sourceName '.' ext];
            if flags.debug
                T.message = [T.message ' (DEBUG)']; end

            T.parameters = {flags,...
                            sources{ii},...
                            headers{ii},...
                            packages{ii},...
                            includes{ii},...
                            libraries{ii},...
                            options{ii}};

            [built{ii}, wME] = T.execute(); %#ok (wME needed for correct nargout)

        end
    end

end

% Decide whether rebuilding a target is necessary
function rebuild = rebuild_necessary(flags,...
                                     source, ...
                                     otherFiles,...
                                     options)

    global DEBUG %#ok<NUSED>

    % Build info store file
    buildinfofile = 'buildinfo.nfo';
    if ~isempty(flags.target_path)
        buildinfofile = fullfile(flags.target_path, buildinfofile);
    else
        buildinfofile = fullfile(pwd, buildinfofile);
    end

    % Initially, assume we have to rebuild
    rebuild = true;

    % Target name
    target = [regexprep(source, '\.[^\.]+$', '') '.' mexext];

    % Get date info on all dependent files
    dirs  = cellfun(@dir, otherFiles);
    dates = sort([dirs.datenum]);

    % Construct current build info structure
    if verLessThan('MATLAB', '8.3')
        targetfield = genvarname(target);
    else
        targetfield = matlab.lang.makeValidName(target);
    end
    buildinfo_skel = struct(targetfield, struct('debug'    , flags.debug,...
                                                'options'  , {options},...
                                                'files'    , otherFiles,...
                                                'dates'    , dates ...
                                                ));
    buildinfo = buildinfo_skel; %#ok<*NASGU> (used in save())

    % Decide whether to do a rebuild
    if ~flags.force_rebuild

        % Look for info on any previous builds for this target
        build_outdated = true;
        if exist(buildinfofile,'file')==2
            try
                buildinfo = load(buildinfofile,'buildinfo','-mat');
                buildinfo = buildinfo.buildinfo;

                if isfield(buildinfo, targetfield) && ...
                        isequal(buildinfo_skel.(targetfield), buildinfo.(targetfield))
                    build_outdated = false;
                else
                    buildinfo.(targetfield) = buildinfo_skel.(targetfield);
                end

            catch ME,ME; %#ok (R2010a mlint bug)
                warning([mfilename() ':buildinfofile_read_failure'], [...
                        'Could not read build information file ''%s''; ',...
                        'the error was\n''%s'''],...
                        buildinfofile, ME.message);
            end
        end

        % Date of the target
        targetDate = -inf;
        if ~isempty(flags.target_path)
            D = dir([flags.target_path filesep target]);
        else
            D = dir(['.' filesep target]);
        end
        if ~isempty(D)
            targetDate = D.datenum; end

        % If any of the dates of the sources, packages, or libraries
        % is newer than the corresponding target, trigger a rebuild
        dependency_outdated = any(dates > targetDate);

        % Also check the MSVC PDB file, if any
        symbols_outdated = false;
        if flags.is_MS && (flags.debug || flags.profiling)
            pdbfile = [flags.target_path filesep target '.pdb'];
            P = dir(pdbfile);
            % (allow 3s difference)
            symbols_outdated =    exist(pdbfile,'file')~=2 ...
                               || abs( P.datenum - targetDate ) > 3;
        end

        % Use all of this info to decide whether to rebuild
        rebuild = build_outdated || dependency_outdated || symbols_outdated;

    end

    % Save current build info
    try
        % NOTE: (Rody Oldenhuis) v6 is faster due to compression in v7 and later
        save(buildinfofile, 'buildinfo', '-mat','-v6');
    catch ME,ME; %#ok (R2010a mlint bug)
        warning([mfilename() ':buildinfofile_read_failure'], [...
                'Could not write build information file ''%s''; ',...
                'the error was\n''%s'''],...
                buildinfofile, ME.message);
    end

end


% (Re)build a MEX file, based on a date comparison of the dependent files
% and the existing MEX file
function [built, wME] = build_mexfile(flags,...
                                      source,...
                                      headers,...
                                      package,...
                                      includes,...
                                      libraries,...
                                      options)

    global DEBUG

    % handy-dandy abbreviation
    uf = {'UniformOutput', false};

    % Default output
    built = '';
    wME   = {};

    % Start build process
    try
        % Target name
        target = [regexprep(source, '\.[^\.]+$', '') '.' mexext];

        % Get the absolute paths of all relevant files, taking into
        % account include dirs
        allFiles = cellstr(char(source,...
                                headers{:},...
                                package{:},...
                                libraries{:}...
                                ));

        fileList     = strcat(pwd, filesep, allFiles);
        validFiles   = cellfun(@(x)exist(x,'file'), fileList)==2;
        invalidFiles = allFiles(~validFiles);
        if ~isempty(invalidFiles)

            % Try files with include dirs
            correctedFiles = invalidFiles;
            for jj = 1:numel(invalidFiles)
                fileWithIncludes = strcat(includes, filesep, invalidFiles{jj});
                tryIncludes      = cellfun(@(x)exist(x,'file'),fileWithIncludes)==2;
                assert(sum(tryIncludes)<=1, [...
                       'Filename ''%s'' occurs multiple times on ',...
                       'given include paths; not making ambiguous build.'],...
                       invalidFiles{jj});
                if ~any(tryIncludes)
                    error([mfilename() ':file_not_found'],...
                          'File ''%s'' not found.', invalidFiles{jj});
                end
                correctedFiles{jj} = fileWithIncludes{tryIncludes};
            end
            allFiles(~validFiles) = correctedFiles;

            % Insert include dirs explicitly (MEX.pl doesn't do this
            % properly for non-header files)
            [whichOnes, whereThen]= ismember(invalidFiles, source);

            if any(whichOnes)
                source(whereThen(whichOnes)) = correctedFiles(whichOnes); end

            [whichOnes,whereThen]= ismember(invalidFiles, package);
            if any(whichOnes)
                package(whereThen(whichOnes)) = correctedFiles(whichOnes); end

            [whichOnes,whereThen]= ismember(invalidFiles, libraries);
            if any(whichOnes)
                libraries(whereThen(whichOnes)) = correctedFiles(whichOnes); end

        end

        % If no rebuild is necessary, we're done
        if ~rebuild_necessary(flags, source, allFiles, options)
            return; end

        % First, clear all MEX files from memory
        clear(target);
        clear mex; %#ok<CLMEX>

        % Checking presence of files and valid options etc. is already
        % handled by MEX; don't do it twice.

        % Make sure extraneous whitespace is gone
        target    = strtrim(target);
        source    = strtrim(source);
        package   = strtrim(package);
        libraries = strtrim(libraries);
        option    = strtrim(options);

        % Options often contain spaces; these should be split into
        % separate cell entries
        inds = cellfun(@(x)any(isspace(x)), option);
        if any(inds)
            option = cellfun(@(x)[x ' '], option, uf{:});
            option = regexp([option{:}], '\s+', 'split');
        end

        % Debug mode
        if flags.debug
            option{end+1} = '-g'; end

        % Include architecture flag
        option{end+1} = ['-' flags.architecture];

        % Build the correct command set for mex()
        include = [cellfun(@(x) ['-I' strtrim(x)],  includes, uf{:})
                   cellfun(@(x) ['-L' strtrim(x)],  includes, uf{:})];
        library = cellfun(@(x) ['-l' strtrim(x)], libraries, uf{:});

        command = { option{:} include{:} source package{:} library{:} }; %#ok<CCAT>
        command = command( ~cellfun('isempty', command) );

        % (make sure the compflag/linkflag is done correctly)
        command = prepare_options(command, 'COMPFLAGS', flags.is_MS);
        command = prepare_options(command, 'LINKFLAGS', flags.is_MS);
        command = ['-silent' command]; %#ok<*NASGU> (used in evalc() below)

        % Compile the MEX-function, passing along the command-line
        % error message OR the internal error message generated by
        % MEX/MEX_HELPER.

        preBuildDate = now();

        % Build!
        try
            cmdOutput = evalc('[~] = mex(command{:})');

        catch mex_ME

            % NOTE: (Rody Oldenhuis) MEX files built on different MATLAB
            % versions sometimes cause this strange error. Take care of it
            % by deleting the MEX file, and re-building it
            if strcmpi(mex_ME.identifier, 'MATLAB:mex:Error') && ...
               ~isempty(strfind(mex_ME.message, ...
                                'is not a MEX file. For more information,'))

                [~,sourcename] = fileparts(source);
                delete([sourcename '.' mexext()]);
                cmdOutput = evalc('[~] = mex(command{:})');

            else
                throw(mex_ME);
            end
        end

        % Re-format output a bit
        cmdOutput = strtrim(cmdOutput);
        cmdOutput = regexprep(cmdOutput, '\\', '\\\\'); % (for paths on Windows)
        cmdOutput = regexprep(cmdOutput, ... % (No way to suppress it, Microsoft?!)
                              '^.*Command line warning D9025.*$', ''); 

        % Build returns text; it may still have succeeded, so don't
        % throw immediately, but do some checks and/or issue warning
        if ~isempty(cmdOutput)
            ME = MException([mfilename() ':got_errorcode'], cmdOutput);

            targetFile = ['.' filesep target];
            if exist(targetFile,'file')==3 &&...
                    subsref(dir(targetFile), substruct('.', 'datenum')) >= preBuildDate
                wME = ME;
            else
                if DEBUG
                    throw(ME); % DEBUG: include full callstack
                else
                    throwAsCaller(ME);
                end
            end
        end

        % Add freshly built source to the list
        [~, targetName] = fileparts(source);
        built = [targetName '.' mexext];

        % Move the freshly built file to the target dir
        if ~isempty(flags.target_path)

            format_msg = @(str) regexprep(regexprep(str,'\\','\\\\'), '%', '%%');
            [success, msg, id] = movefile(built, flags.target_path);

            % Build still has succeeded; don't throw, but issue warning
            if ~success
                wME = [wME; MException(id,format_msg(msg))]; end

            % (for debug builds with MSVS, also move the PDB file)
            pdbfile = [pwd filesep built '.pdb'];
            if flags.is_MS && ...
                    (flags.debug || flags.profiling) && ...
                    exist(pdbfile,'file')==2
                [success, msg,id] = movefile(pdbfile, flags.target_path);
                if ~success
                    wME = [wME; MException(id,format_msg(msg))]; end
            end
        end


    catch ME

        ME_base = MException([mfilename() ':general_failure'], ...
                             'Could not complete build process successfully.');
        ME_main = addCause(ME_base, ME);
        ME_final = MException([mfilename() ':some_binaries_not_created'],[...
                              '\n\nNot all binaries could be created ',...
                              'successfully.\nPlease correct all errors ',...
                              'indicated above, and run %s() again.'],...
                              mfilename());
        ME = addCause(ME_main, ME_final);

        if DEBUG
            throw(ME); % DEBUG: include full stack
        else
            throwAsCaller(ME);
        end

    end

end


% - Include the previous options in an option set. For example: make sure
%   that 'COMFLAGS="(option list)"' includes any previous flags:
%   'COMFLAGS="$COMFLAGS (option list)"'
% - suppress logos and any branding
function command = prepare_options(command, option, supress_logo)
    mtc = regexp(command, [option '="*'], 'match', 'once');
    ind = find( ~cellfun('isempty', mtc) );
    if ~isempty(ind)
        new_addition = {['$' option]};
        if supress_logo
            new_addition = [new_addition '/nologo']; end
        command = [command(1:ind), new_addition, command(ind+1:end)];
    end
end


% Determine what happens on successful task completion
function finalize(terminateTask, built,wME)
    if isempty(wME)
        if isempty(built)
            terminateTask(Tasking.ExitStatus.NOOP);
        else
            terminateTask(Tasking.ExitStatus.COMPLETED);
        end
    else
        terminateTask(Tasking.ExitStatus.WARNING);
        if ~iscell(wME)
            wME = {wME}; end

        warning off backtrace
        cellfun(@(x) warning(regexprep(x.message, '\\', '\\')), wME);
        warning on backtrace

        fprintf(1, '\n\n');
    end
end

