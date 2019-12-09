% Space Image Format

% returns a list of layers, each layer is a list of Height pixel rows,
% with Width entries in each row
read_siffile(SIFData, Width, Height, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    string_chars(Line, LineArray),
    layers(LineArray, Width, Height, SIFData).

layers([], _, _, []).
layers(Line, Width, Height, [LayerData|RemLayerData]) :-
    LayerLen is Width * Height,
    append(Layer, RemLine, Line),
    length(Layer, LayerLen),
    layer(Layer, Width, LayerData),
    length(LayerData, Height),  % just a verification
    layers(RemLine, Width, Height, RemLayerData).

layer([], _, []).
layer(Layer, RowSize, [RowData|RemRows]) :-
    append(Row, RemLayer, Layer),
    length(Row, RowSize),
    intcodes(Row, RowData),
    layer(RemLayer, RowSize, RemRows).

intcodes([], []).
intcodes([C|CS], [I|IS]) :-
    atom_string(C, S),
    number_string(I, S),
    intcodes(CS, IS).

% ----------------------------------------------------------------------

num_digits_row(Digit, RowData, NZeros) :-
    delete(RowData, Digit, RemData),
    length(RowData, L1),
    length(RemData, L2),
    NZeros is L1 - L2.

num_digits_layer(_, [], 0).
num_digits_layer(Digit, [RowData|RemRows], NZeros) :-
    num_digits_row(Digit, RowData, NZRow),
    num_digits_layer(Digit, RemRows, NZRem),
    NZeros is NZRow + NZRem.

min_zeros_layer([LayerData|RemLayers], Layer) :-
    num_digits_layer(0, LayerData, NZLayer),
    min_zeros_layer(RemLayers, LayerData, NZLayer, Layer).

min_zeros_layer([], Layer, _, Layer).
min_zeros_layer([LayerData|RemLayers], PrevLayer, PrevZeros, Layer) :-
    num_digits_layer(0, LayerData, NZLayer),
    (PrevZeros =< NZLayer ->
         min_zeros_layer(RemLayers, PrevLayer, PrevZeros, Layer)
    ;
    min_zeros_layer(RemLayers, LayerData, NZLayer, Layer)).
    
% ----------------------------------------------------------------------

test1 :- SIFData = [ [ [1,2,3], [4,5,6] ], [ [7,8,9], [0,1,2] ] ],
         read_siffile(SIFData, 3, 2, '../../inputs/day8/testinp1'),
         min_zeros_layer(SIFData, [ [1,2,3], [4,5,6] ]).
         

test2 :- SIFData = [ [ [1,0,2,0,3], [4,0,5,0,6] ], [ [7,1,8,2,9], [0,1,2,3,4] ] ],
         read_siffile(SIFData, 5, 2, '../../inputs/day8/testinp2'),
         min_zeros_layer(SIFData, MinZLayer),
         MinZLayer = [ [7,1,8,2,9], [0,1,2,3,4] ],
         num_digits_layer(1, MinZLayer, Num1),
         num_digits_layer(2, MinZLayer, Num2),
         4 is Num1 * Num2.

test3 :- read_siffile(SIFData, 2, 2, '../../inputs/day8/testinp3'),
         decode_sif(SIFData, [ [ 0, 1 ], [ 1, 0 ] ]).

test3_render :-
    render_sif('../../inputs/day8/testinp3', 2, 2).

test :- test1, test2, test3.

% ----------------------------------------------------------------------

validate_sif(FName, Width, Height, ValidationValue) :-
    read_siffile(SIFData, Width, Height, FName),
    min_zeros_layer(SIFData, MinZLayer),
    num_digits_layer(1, MinZLayer, Num1),
    num_digits_layer(2, MinZLayer, Num2),
    ValidationValue is Num1 * Num2.

% ----------------------------------------

decode_sif([Layer1|Layers], Decoded) :-
    decode_layers(Layer1, Layers, Decoded).

decode_layers(Current, [Layer|Layers], Decoded) :-
    decode_layer(Current, Layer, LayerDecode),
    decode_layers(LayerDecode, Layers, Decoded).
decode_layers(Decoded, [], Decoded).

decode_layer([Row|Rows], [RowUnder|RUS], [DcdRow|DcdRows]) :-
    decode_row(Row, RowUnder, DcdRow),
    decode_layer(Rows, RUS, DcdRows).
decode_layer([], [], []).

decode_row([C|CS], [U|US], [D|DS]) :-
    (C =:= 2 -> D = U ; D = C),
    decode_row(CS, US, DS).
decode_row([], [], []).
    
% ----------------------------------------

render_sif(FName, Width, Height) :-
    read_siffile(SIFData, Width, Height, FName),
    decode_sif(SIFData, Decoded),
    render_layers(Decoded).
render_layers([Layer|RemLayers]) :-
    render_layer(Layer),
    render_layers(RemLayers).
render_layers([]).

render_layer([V|VS]) :-
    (V =:= 1 -> format('#') ; format(' ')),
    %% number_string(V, S),
    %% format('~s',[S]),
    render_layer(VS).
render_layer([]) :- format('~n').
