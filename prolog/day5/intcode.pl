read_intcode(Memory, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    string_to_intcode(Line, Memory).

string_to_intcode(Line, Program) :-
    split_string(Line, ",", "", Codes),
    intcodes(Codes, Program).

intcodes([], []).
intcodes([C|CS], [I|IS]) :-
    number_string(I, C),
    intcodes(CS, IS).

% ----------------------------------------------------------------------

run_program(Memory, Inp, FinalMem, Out) :-
    run_opcode(state(Memory, 0, Inp, [], run), state(FinalMem, _, _, Out, halt)).

run_opcode(state(Memory, IP, Inp, Out, run), FinalState) :-
    nth0(IP, Memory, Instruction),
    opcode(Instruction, Opcode, Modes),
    succ(IP, Param0Ptr),
    run_opcode(Opcode, Modes, state(Memory, Param0Ptr, Inp, Out, run), UpdState),
    run_opcode(UpdState, FinalState).
run_opcode(state(Memory, _, _, Out, halt), state(Memory, _, _, Out, halt)).

opcode(Instruction, Opcode, Modes) :- divmod(Instruction, 100, Modes, Opcode).
mode(Modes, Mode, RemModes) :- divmod(Modes, 10, RemModes, Mode).


% ----------------------------------------------------------------------

%% pi(P).  % input parameter
%% po(P).  % output parameter

param(0, PAddr, Memory, pi(P)) :- readmem(PAddr, Memory, Ptr), readmem(Ptr, Memory, P).
param(1, PAddr, Memory, pi(P)) :- readmem(PAddr, Memory, P).

param(0, PAddr, Memory, po(P)) :- readmem(PAddr, Memory, P).
% param(0, PAddr, Memory, po(P)) % not supported


params(Modes, PAddr, Memory, P1, RModes, NextAddr) :-
    mode(Modes, Mode, RModes),
    param(Mode, PAddr, Memory, P1),
    succ(PAddr, NextAddr).
params(Modes, PAddr, Memory, P1, P2, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, P1, Mode1, Next),
    params(Mode1, Next, Memory, P2, RModes, NextAddr).
params(Modes, PAddr, Memory, P1, P2, P3, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, P1, P2, Mode1, Next),
    params(Mode1, Next, Memory, P3, RModes, NextAddr).

writemem(Addr, Val, Memory, UpdMemory) :-
    nth0(Addr, Memory, _, M1),
    nth0(Addr, UpdMemory, Val, M1).

readmem(Addr, Memory, Val) :-
    nth0(Addr, Memory, Val, _).

% ----------------------------------------------------------------------

run_opcode(99, _,
           state(Memory, _, _, Out, _),
           state(Memory, _, _, Out, halt)).  % HALT

run_opcode(1,  Modes,
           state(Memory,   PAddr,  Inp, Out, run),
           state(FinalMem, NextIP, Inp, Out, run)) :-  % ADD
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    plus(P1, P2, Res),
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(2, Modes,
           state(Memory,   PAddr,  Inp, Out, run),
           state(FinalMem, NextIP, Inp, Out, run)) :-  % MUL
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    Res is P1 * P2,
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(3, Modes,
           state(Memory, PAddr,  Inp,    Out, run),
           state(UpdMem, NextIP, RemInp, Out, run)) :- % INP
    params(Modes, PAddr, Memory, po(Ptr), _, NextIP),
    nth0(0, Inp, InpVal, RemInp),
    writemem(Ptr, InpVal, Memory, UpdMem).

run_opcode(4, Modes,
           state(Memory, PAddr,  Inp, Out,    run),
           state(Memory, NextIP, Inp, NewOut, run)) :- % OUT
    params(Modes, PAddr, Memory, pi(P1), _, NextIP),
    append(Out, [P1], NewOut).

run_opcode(5, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(Memory, NextIP, Inp, Out, run)) :- % JNZ
    params(Modes, PAddr, Memory, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> (NextIP is StepIP) ; (NextIP is DestIP)).

run_opcode(6, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(Memory, NextIP, Inp, Out, run)) :- % JEQ
    params(Modes, PAddr, Memory, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> NextIP is DestIP ; NextIP is StepIP).

run_opcode(7, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(UpdMem, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr), _, NextIP),
    ((P1 < P2 ->
          writemem(Ptr, 1, Memory, UpdMem)) ;
     writemem(Ptr, 0, Memory, UpdMem)).

run_opcode(8, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(UpdMem, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr), _, NextIP),
    (P1 =:= P2 ->
         writemem(Ptr, 1, Memory, UpdMem) ;
     writemem(Ptr, 0, Memory, UpdMem)).


% ----------------------------------------------------------------------

test1() :-
    read_intcode(Program, "../../inputs/day2/testinp1"),
    read_intcode(Result, "../../inputs/day2/testout1"),
    run_program(Program, _, Final, _),
    Result == Final.

test2() :-
    read_intcode(Program, "../../inputs/day2/testinp2"),
    read_intcode(Result, "../../inputs/day2/testout2"),
    run_program(Program, _, Final, _),
    Result == Final.

test3() :-
    read_intcode(Program, "../../inputs/day2/testinp3"),
    read_intcode(Result, "../../inputs/day2/testout3"),
    run_program(Program, _, Final, _),
    Result == Final.

test4() :-
    read_intcode(Program, "../../inputs/day2/testinp4"),
    read_intcode(Result, "../../inputs/day2/testout4"),
    run_program(Program, _, Final, _),
    Result == Final.

test5() :-
    read_intcode(Program, "../../inputs/day2/testinp5"),
    read_intcode(Result, "../../inputs/day2/testout5"),
    run_program(Program, _, Final, _),
    Result == Final.

test6() :-
    between(0, 12, X),
    (X =:= 8 -> R is 1; R is 0),
    run_program([3,9,8,9,10,9,4,9,99,-1,8], [X], _, [R]).

test7() :-
    between(0, 12, X),
    (X < 8 -> R is 1; R is 0),
    run_program([3,9,7,9,10,9,4,9,99,-1,8], [X], _, [R]).

test8() :-
    between(0, 12, X),
    (X =:= 8 -> R is 1; R is 0),
    run_program([3,3,1108,-1,8,3,4,3,99], [X], _, [R]).

test9() :-
    between(0, 12, X),
    (X < 8 -> R is 1; R is 0),
    run_program([3,3,1107,-1,8,3,4,3,99], [X], _, [R]).

test10() :-
    between(0, 12, X),
    (X =:= 0 -> R is 0; R is 1),
    run_program([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], [X], _, [R]).

test11() :-
    between(0, 12, X),
    (X =:= 0 -> R is 0; R is 1),
    run_program([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], [X], _, [R]).

test12() :-
    between(0, 12, X),
    (X =:= 8 -> R is 1000; (X < 8 -> R is 999; R is 1001)),
    run_program([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
                ], [X], _, [R]).
    
test() :- test1, test2, test3, test4, test5, test6, test7, test8, test9,
          test10, test11, test12.

% ----------------------------------------------------------------------

program(FName, Noun, Verb, Result) :-
    read_intcode(Program, FName),
    between(0, 99, Noun),
    between(0, 99, Verb),
    writemem(1, Noun, Program, P1),
    writemem(2, Verb, P1, P2),
    run_program(P2, _, Final, _),
    readmem(0, Final, Result).

diagnostic(FName, InpVal, Result) :-
    read_intcode(Program, FName),
    run_program(Program, [InpVal], _, Result).
