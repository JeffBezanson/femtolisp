function varargout = textread(filename, format, varargin)
% textread Formatted file read to one or more variables
%
% Syntax:
% [<variable-1> <<,variable-2>> <<,variable-N>> ] = ...
%                               textread( <input-filename>, <format-specifiers> <<,number-of-lines-to-read>> )
%
% This function is available for task parallel processing.
%
% ************
%
% The Star-P M implementation of this function exhibits the same signature and output characteristics as the Matlab 
% function of the same name.
% 
% For details, please see matlab:textread. 
%
% ************************
%
%

% DOC % A
	if nargin < 2, error('Not enough input arguments.'); end

	if ~ischar(filename), error('Filename must be a string.'); end

	ifExist = exist(filename, 'file');
	if ifExist ~= 2 && ifExist ~= 4, error('File not found'); end

	fid = fopen(filename, 'r');
	if fid == -1, error(lasterror()); end;
   
    formatin = formatread(format);
    argin = readvarargin(varargin{:});

    
    % Проверка количества исходящих аргументов
    count = 0;
    for k = 1:length(formatin),
        if ~isequal(formatin(k).symbol, '*'), count = count + 1; end;
    end
    if count ~= nargout, error('widthber of outputs must match the widthber of unskipped input fields.');end
       
    % Флаг flag_N - опредиляет сколько раз использовать строку формата 
    % (N или пока не считаем весь файл)
    flag_N = 1; 
    if ~isempty(argin.N)
        N = argin.N;
    else
        N = 1; flag_N = 0;
    end
    
    % Пропустить первые N == headerlines линий
    for i = 1:argin.headerlines
        text = fgets(fid);
    end
    
    % Если строка пустая считать следующую
    text = fgets(fid);
        
    t = 1;
    k = 1;
    
    maxlen = 1;
    vararginEmpty = 1;
    
    while N
        
        t = 1;
        if ~isempty(format)
            if passLine(text, argin)
                for j = 1:length(formatin)
                    s = formatin(j);
                    
                    if s.type == 'c' && isempty(text)
                        while 1
                            text = fgets(fid);
                            if ~ischar(text)
                                fclose(fid);
                                return;
                            else
                                if ~(text(1) == 13)
                                    break;
                                end
                            end
                        end
                    end
                        

                    % Удалить первые лишние пробелы
                    text = removeAllFirstSpaces(text, argin.delimiter);
                    % Считать следующее слово указанного типа
                    [out, text] =  switchType(text, s, argin, fid);
                    % Пропустить слово если установлен параметр *

                    if ~isequal(s.symbol, '*')
                        if ~isempty(text) || ~(isempty(out) || isequal(out, {''}))
                            out = setEmptyValue(out, s, argin);                        
                            if vararginEmpty
                                varargout{t}(1, :) = out;
                            else
                                varargout{t}(end + 1, :) = out;                            
                            end
                        end
                        t = t + 1; 
                    end;

                    % Убрать первый символ если он равен delimiter 
                    if ~isempty(argin.delimiter) && ~isempty(text) && isa(text, 'char')
                        if find(argin.delimiter == text(1))
                            text = text(2:end);
                        end
                    end;
                end
                vararginEmpty = 0;
            end
        else % Если строка формата не задана читать как double
            
            if passLine(text, argin)
                [out, text] = readDoubleArray(text, argin);
                curmaxlen = maxlen;
                if length(out) > maxlen, maxlen = length(out); end;
                for z = 1:k 
                    for q = curmaxlen+1:maxlen
                        varargout{1}(z, q) = argin.emptyvalue;
                    end
                end
                for q = length(out)+1:maxlen
                    out(q) = argin.emptyvalue;
                end

                varargout{1}(k, :) = out;
                k = k + 1;
            end
            
            
        end
        
        text = removeAllFirstSpaces(text, argin.delimiter);
        % Если строка пустая считать следующую
        if isempty(text)
            text = fgets(fid);
        elseif find(text(1) == [10 13])
            text = fgets(fid);
        end
        % Выйти если не смогли считать строку
        if ~ischar(text), break; end;
        
        if flag_N, N = N - 1; end;
    end
   
    fclose(fid);
    
end

% -------- Работа с текстом ---------------------------

% Удаляет все первые разделители
function text = removeAllFirstSpaces(text, delimiter)
    %if ~isempty(delimiter), return; end;
    idx = [];
    for k = 1:length(text)
        idx = find(text(k) ~= ' ', 1);
        if ~isempty(idx), break; end;
    end
	if ~isempty(idx)
        text = text(k:end);
    else
        text = '';
	end
end

% Читает первые n - символов
function [word, text] = readCharacters(text, n, fid)
    word = '';
    while n
        if n > length(text)
            word = [word text(1:end)];
            n = n - length(text);
            text = fgets(fid);
            if ~ischar(text), error(sprintf('Trouble reading characters from file: %s', text)); end
        else
            word = [word text(1:n)];
            text = text(n+1:end);
            n = 0;
        end 
    end
end

% Читает первое слово до разделитель или первые n - символов
function [word, text] = readString(text, n, delimiter)
    if isempty(delimiter), delimiter = [13, 10, 32]; 
    else
        delimiter = [delimiter, 13, 10];
    end
    
    word = '';
    if isempty(n) || n > length(text) , n = length(text); end;
    for k = 1:n
        if find(delimiter == text(k))
            word = text(1:k-1);
            text = text(k:end);
            return;
        end
    end
    word = text(1:k);
    text = text(k+1:end);
    
end

% Читает первые числа до разделителяили или первые n - символов
function [word, text] = readNumber(text, n)

    if isempty(text), word = ''; end;
    
    word = [];
    if isempty(n) ||  length(text) < n, n = length(text); end;
    
    for k = 1:n
       if text(k) < 48 || text(k) > 57
           word = text(1:k-1);
           text = text(k:end);
           return;
       end
    end
    word = text(1:k);
    text = text(k+1:end);
    
end

% Читает число с точкой до разделителяили или первые n - символов
function [word, text] = readFloat(text, s)
    
    if isempty(text), word = ''; return; end;
    
    if isempty(s), s.width = []; s.precision = []; end;
    
    if isempty(s.width) ||  length(text) < s.width
        n = length(text); 
    else
        n = s.width;
    end;
    
    if isempty(s.precision), s.precision = n; end;
    
    % Чтение знака
    [sign, text] = getSign(text);
    if ~isempty(sign), n = n - 1; end;

    point = 0;
    npoint = 0;
    word = sign;
    for k = 1:n
        if point
            npoint = npoint + 1;
        end
        if text(k) == '.' && ~point
            point = 1;
            continue;
        end
        if text(k) < 48 || text(k) > 57 || npoint > s.precision
            word = [word text(1:k-1)];
            text = text(k:end);
            return;
        end
    end
    word = [word text(1:k)];
    text = text(k+1:end);
    
end

% Определяет знак
function [sign, text] = getSign(text)
    if isempty(text), sign = ''; return; end;
    if text(1) == '+' || text(1) == '-'
        sign = text(1);
        text = text(2:end);
        if isempty(text) || text(1) < 48 || text(1) > 57, error(sprintf('Trouble reading double from file: %s', text)); end;
    else
        sign = [];
    end
end

% 0 - пропустить строку, 1 - обрабатывать
function out = passLine(text, argin)

    isdelimiter = 0;
    if argin.delimiter
        if ~isempty(find(text == argin.delimiter, 1))
            isdelimiter = 1;
        end
    end
    
    isnewline = 0;
    if ~isempty(find(text(1) == [10 13], 1))
        isnewline = 1;
    end 
    if ~isnewline || isdelimiter
        out = 1;
    else
        out = 0;
    end
    
end


% -------- Парс входящих параметров ---------------------------

% Читает входящие параметры в структуру
function argin = readvarargin(varargin)

    
    argin = struct();
    argin(1).N = [];
    argin(1).bufsize = 4095;
    argin(1).commentstyle = [];
    argin(1).delimiter = '';
    argin(1).emptyvalue = 0;
    argin(1).endofline = [];
    argin(1).expchars = [];
    argin(1).headerlines = 0;
    argin(1).whitespace = [];
    
    if nargin == 0, return; end;

    k = 1;
    if isnumeric(varargin{1})
        argin.N = varargin{1};
        k = k + 1;
    end
   

    count = (length(varargin(k:end)) / 2);
    if floor(count) - count ~= 0, error('Param/value pairs must come in pairs'); end;
    
    while k < nargin
        switch varargin{k}
            
            case 'bufsize'
                k = k + 1;
                if isinteger(varargin{k}) && isscalar(varargin{k})
                    argin(1).bufsize = str2double(varargin{k});
                else
                    error('Buffer size must be a scalar integer.');
                end
                
            case 'commentstyle'
                k = k + 1;
                switch varargin{k}
                    case 'matlab'
                        argin(1).commentstyle = '%';
                    case 'shell'
                        argin(1).commentstyle = '#';
                    case 'c++'
                        argin(1).commentstyle = '//';
                    otherwise
                        error('Invalid comment style.');
                end
                
            case 'delimiter'
                k = k + 1;
                switch varargin{k}
                    case '\n'
                        num = 10;
                    case '\r'
                        num = 13;
                    otherwise
                        num = double(varargin{k});
                end
                argin(1).delimiter = num;
                
            case 'emptyvalue'
                k = k + 1;
                if isnumeric(varargin{k}) && isscalar(varargin{k})
                    argin(1).emptyvalue = varargin{k};
                else
                    error('Emptyvalue must be a scalar double.');
                end              
                 
            case 'endofline'
                k = k + 1;
                if ischar(varargin{k})
                    argin(1).endofline = varargin{k};
                else
                    error('endofline must be a scalar double.');
                end   
                
            case 'expchars'
                
            case 'headerlines'
                k = k + 1;
                if isnumeric(varargin{k}) && isscalar(varargin{k})
                    argin(1).headerlines = varargin{k};
                else
                    error('Headerlines must be a scalar integer.');
                end              
                
            case 'whitespace'  
                
            otherwise
                error('Unknown option');
        end
        
        k = k + 1;
        
    end
    
end

% Читает строку формата в структуру
function R = formatread(format)

    formatType = ['d', 'u', 'f', 's', 'q', 'c'];
    k = 1;
    t = 1;
    s = struct();
    s(t).type = [];
    s(t).width = [];
    s(t).precision = [];
    s(t).symbol = [];
    s(t).text = [];
    
	while ~isempty(format) 
        
        type = [];
        width = [];
        precision = [];
        symbol = [];
        text = [];
        
        format = removeAllFirstSpaces(format, '');
        if format(1) == '%'
            format = format(2:end);
            
            
            if format(1) == '*'
                symbol = '*'; 
                format = format(2:end);
            end;
            
            [width, format] = readNumber(format, []);
            if format(1) == '.'
                format = format(2:end);
                [precision, format] = readNumber(format, []);
            end
            
            type = format(1);
            format = format(2:end);
            
            % Check and save correct format
            idx = find( formatType == type );
            if isempty(idx)
                error('Incorrect format'); 
            end;
            
            % Save width
            if ~isempty(width), width = str2double(width);end;
            % Save precision
            if ~isempty(precision), precision = str2double(precision);end;
            
        else
            
            [text, format] = readString(format, [], [' ', '%']);
            symbol = '*';
            type = 'r';
        end
        
        s(t).type = type;
        s(t).width = width;
        s(t).precision = precision;
        s(t).symbol = symbol;
        s(t).text = text;
        
        t = t + 1;
        
	end
    
    R = s;
    
end

% ------------- Вспомагательные функции --------------------

function [out, text] = switchType(text, s, argin, fid)

    switch s.type

        case 'd'
            width = s.width;
            % Чтение знака числа
            [sign, text] = getSign(text);
            if ~isempty(sign), width = width - 1; end;
            % Чиение числа
            [word, text] = readNumber(text, width);
            % Обьеденить знак и число
            out = [sign word];
            % Если опция emptyvalue установлена и число пустое то заменить на заданное
            if ~isempty(out)
                out = str2double(out);
                if isequalwithequalnans(out, NaN), error(sprintf('Trouble reading double from file: %s', text)); end;
            else
                if ~isempty(text) && isempty(find(text(1) == [13, 10], 1))
                    error(sprintf('Trouble reading integer from file: %s', text));
                end
            end

        case 'u'
            if isempty(text) || ~isempty(find(text(1) == [13, 10], 1))
                out = []; return;
            end            
            [out, text] = readNumber(text, s.width);
            % Если опция emptyvalue установлена и число пустое то заменить на заданное
            if ~isempty(out)
                out = str2double(out);
                if isequalwithequalnans(out, NaN), error(sprintf('Trouble reading integer from file: %s', text)); end;
            else
                if ~isempty(text) && isempty(find(text(1) == [13, 10], 1))
                    error(sprintf('Trouble reading integer from file: %s', text));
                end
            end
            
        case 'f'
            % Чтение числа
            [out, text] = readFloat(text, s);
            % Если опция emptyvalue установлена и число пустое то заменить на заданное
            if ~isempty(out)
                out = str2double(out);
                if isequalwithequalnans(out, NaN), error(sprintf('Trouble reading double from file: %s', text)); end;
            else
                if ~isempty(text) && isempty(find(text(1) == [13, 10], 1))
                    error(sprintf('Trouble reading integer from file: %s', text));
                end
            end

        case 's'
            [word, text] = readString(text, s.width, argin.delimiter);
            if isempty(word)
                out = {''};
            else
                out = {word};
            end

        case 'q'

        case 'c'
            n = 1;
            if ~isempty(s.width), n = s.width; end;
            [word, text] = readCharacters(text, n, fid);
            out = word(:);

        case 'r'
            [out, text] = readCharacters(text, length(s.text));
            if ~isequal(out, s.text), error('Trouble reading characters from file'); end;

        otherwise
            error('Error');
    end

end

function out = setEmptyValue(text, s, argin)
    out = text;
    if isempty(text)
        if find(['d', 'u', 'f'] == s.type)
            out = argin.emptyvalue;
        end
    end
end

function [out, text] = readDoubleArray(text, argin)

    if isempty(text); out = []; return; end;
    t = 1;
    while isempty(find(text(1) == [13 10], 1))
        % Чтение знака
        [sign, text] = getSign(text);
        % Чтение числа
        [word, text] = readFloat(text, []);
        % Обьеденить знак и число
        word = [sign word];
        % Если опция emptyvalue установлена и число пустое то заменить на заданное
        if ~isempty(argin.emptyvalue) && isempty(word)
            out(t) = argin.emptyvalue;
        else
            out(t) = str2double(word);
            if isequalwithequalnans(out(t), NaN), error('Trouble reading integer from file'); end;
        end
        
        % Убрать первый символ если он равен delimiter 
        if ~isempty(argin.delimiter) && ~isempty(text)
            if find(argin.delimiter == text(1))
                text = text(2:end);
            end
        end;
                
        t = t + 1;
        if isempty(text); break; end;
    end
    
end


