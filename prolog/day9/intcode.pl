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

run_program(Memory, Inp, FinalState, Out) :-
    run_opcode(state(Memory, 0, 0, Inp, [], run), FinalState), !,
    output(FinalState, Out).

continue_program(state(Memory, RelBase, IP, _, _Out0, pause), Inp, FinalState, Out) :-
    run_opcode(state(Memory, RelBase, IP, Inp, [], run), FinalState), !,
    output(FinalState, Out).

run_program_to_end(Memory, Inp, FinalState, Out) :-
    run_opcode(state(Memory, 0, 0, Inp, [], run), UpdState), !,
    run_program_to_end(UpdState, FinalState, Out).
run_program_to_end(state(Memory, RelBase, IP, Inp, Out0, pause), FinalState, Out) :-
    run_opcode(state(Memory, RelBase, IP, Inp, Out0, run), UpdState), !,
    run_program_to_end(UpdState, FinalState, Out).
run_program_to_end(state(Memory, RelBase, IP, Inp, Out, halt), FinalState, Out) :-
    FinalState = state(Memory, RelBase, IP, Inp, Out, halt).

%%     continue_program(UpdState, FinalState),
%%     output(FinalState, Out).
                     
%% continue_run_program(State, FinalState) :-
%%     continue_program(State, [], UpdState, Out) ->
%%         continue_run_program(UpdState, FinalState)
%%     ; FinalState = State.

run_opcode(state(Memory, RelBase, IP, Inp, Out, run), FinalState) :-
    nth0(IP, Memory, Instruction),
    opcode(Instruction, Opcode, Modes),
    succ(IP, Param0Ptr),
    run_opcode(Opcode, Modes, state(Memory, RelBase, Param0Ptr, Inp, Out, run), UpdState), !,
    run_opcode(UpdState, FinalState).
run_opcode(state(Memory, RelBase, IP, Inp, Out, halt), state(Memory, RelBase, IP, Inp, Out, halt)).
run_opcode(state(Memory, RelBase, IP, Inp, Out, pause), state(Memory, RelBase, IP, Inp, Out, pause)).

opcode(Instruction, Opcode, Modes) :- divmod(Instruction, 100, Modes, Opcode).
mode(Modes, Mode, RemModes) :- divmod(Modes, 10, RemModes, Mode).

memory(state(Memory, _, _, _, _, _), Memory).
output(state(_, _, _, _, Output, _), Output).

can_continue_state(state(_, _, _, _, _, pause)).

% ----------------------------------------------------------------------

%% pi(P).  % input parameter
%% po(P).  % output parameter

% param(Mode, ParamAddr, Memory, RelBase, ParamValue)
%   Mode = 0  -- position (use Memory[Memory[PAddr]])
%   Mode = 1  -- immediate (use Memory[PAddr] directly)
%   Mode = 2  -- relative (use Memory[RelativeBase + Memory[PAddr]])
param(0, PAddr, Memory, _RelBase, pi(P)) :- readmem(PAddr, Memory, Ptr), readmem(Ptr, Memory, P).
param(1, PAddr, Memory, _RelBase, pi(P)) :- readmem(PAddr, Memory, P).
param(2, PAddr, Memory, RelBase,  pi(P)) :- readmem(PAddr, Memory, Ptr), plus(Ptr, RelBase, TgtAddr), readmem(TgtAddr, Memory, P).

param(0, PAddr, Memory, _RelBase, po(P)) :- readmem(PAddr, Memory, P).
% param(0, PAddr, Memory, po(P)) % not supported
param(2, PAddr, Memory, RelBase,  po(P)) :- readmem(PAddr, Memory, POff), plus(POff, RelBase, P).


params(Modes, PAddr, Memory, RelBase, P1, RModes, NextAddr) :-
    mode(Modes, Mode, RModes),
    param(Mode, PAddr, Memory, RelBase, P1),
    succ(PAddr, NextAddr).
params(Modes, PAddr, Memory, RelBase, P1, P2, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, RelBase, P1, Mode1, Next),
    params(Mode1, Next, Memory, RelBase, P2, RModes, NextAddr).
params(Modes, PAddr, Memory, RelBase, P1, P2, P3, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, RelBase, P1, P2, Mode1, Next),
    params(Mode1, Next, Memory, RelBase, P3, RModes, NextAddr).

writemem(Addr, Val, Memory, UpdMemory) :-
    length(Memory, Memsize),
    (Addr < Memsize ->
         (nth0(Addr, Memory, _, M1),
          nth0(Addr, UpdMemory, Val, M1))
    ; (FillSize is Addr - Memsize,
       %% length(Fill, FillSize),
       %% findall(I, (between(0, FillSize, I), nth0(I, Fill, 0)), _),
       zeromem([], FillSize, Fill),
       append(Memory, Fill, M1),
       append(M1, [Val], UpdMemory))).

zeromem(I, N, I) :- length(I, N).
zeromem(I, N, O) :- length(I, M), M < N, zeromem([0|I], N, O).

readmem(Addr, Memory, Val) :-
    length(Memory, Memsize),
    (Addr < Memsize ->
         nth0(Addr, Memory, Val, _)
    ; Val = 0).

% ----------------------------------------------------------------------

run_opcode(99, _,
           state(Memory, _, _, _, Out, _),
           state(Memory, _, _, _, Out, halt)).  % HALT

run_opcode(1,  Modes,
           state(Memory,   RelBase, PAddr,  Inp, Out, run),
           state(FinalMem, RelBase, NextIP, Inp, Out, run)) :-  % ADD
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    plus(P1, P2, Res),
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(2, Modes,
           state(Memory,   RelBase, PAddr,  Inp, Out, run),
           state(FinalMem, RelBase, NextIP, Inp, Out, run)) :-  % MUL
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    Res is P1 * P2,
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(3, Modes,
           state(Memory, RelBase, PAddr,  Inp,    Out, run),
           state(UpdMem, RelBase, NextIP, RemInp, Out, run)) :- % INP
    params(Modes, PAddr, Memory, RelBase, po(Ptr), _, NextIP),
    nth0(0, Inp, InpVal, RemInp),
    writemem(Ptr, InpVal, Memory, UpdMem).

run_opcode(4, Modes,
           state(Memory, RelBase, PAddr,  Inp,    Out, run),
           state(Memory, RelBase, NextIP, Inp, NewOut, pause)) :- % OUT
    params(Modes, PAddr, Memory, RelBase, pi(P1), _, NextIP),
    append(Out, [P1], NewOut).

run_opcode(5, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(Memory, RelBase, NextIP, Inp, Out, run)) :- % JNZ
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> (NextIP is StepIP) ; (NextIP is DestIP)).

run_opcode(6, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(Memory, RelBase, NextIP, Inp, Out, run)) :- % JEQ
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> NextIP is DestIP ; NextIP is StepIP).

run_opcode(7, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(UpdMem, RelBase, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr), _, NextIP),
    ((P1 < P2 ->
          writemem(Ptr, 1, Memory, UpdMem)) ;
     writemem(Ptr, 0, Memory, UpdMem)).

run_opcode(8, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(UpdMem, RelBase, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr), _, NextIP),
    (P1 =:= P2 ->
         writemem(Ptr, 1, Memory, UpdMem) ;
     writemem(Ptr, 0, Memory, UpdMem)).

run_opcode(9, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(Memory, NewRelB, NextIP, Inp, Out, run)) :- % SetRelBase
    params(Modes, PAddr, Memory, RelBase, pi(P1), _, NextIP),
    NewRelB is RelBase + P1.


% ----------------------------------------------------------------------

test18 :- read_intcode(Program, '../../inputs/day9/testinp1'),
          run_program_to_end(Program, [], _, Program).

test19 :- read_intcode(Program, '../../inputs/day9/testinp2'),
          run_program_to_end(Program, [], _, [1219070632396864]).

test20 :- read_intcode(Program, '../../inputs/day9/testinp3'),
          run_program_to_end(Program, [], _, [1125899906842624]).
    
test() :- test18, test19, test20.

% ----------------------------------------------------------------------

boost(FName, Inp, Result) :-
    read_intcode(Program, FName),
    run_program_to_end(Program, [Inp], _, [Result|_]).
