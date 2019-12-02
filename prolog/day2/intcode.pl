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

run_program(Memory, FinalMem) :-
    run_step(Memory, 0, FinalMem).

run_step(Memory, IP, FinalMem) :-
    nth0(IP, Memory, Opcode),
    succ(IP, Param0Ptr),
    run_opcode(Opcode, Param0Ptr, Memory, UpdMemory, IPInc),
    next_step(UpdMemory, IP, IPInc, FinalMem).

next_step(stop(Memory), _, _, Memory).
next_step(continue(Memory), IP, IPInc, FinalMem) :-
    plus(IP, IPInc, NextIP),
    run_step(Memory, NextIP, FinalMem).

run_opcode(99, _, Memory, stop(Memory), 0).

run_opcode(Opcode, Param0Ptr, Memory, continue(FinalMem), IPInc) :-
    readmem(Param0Ptr, Memory, Param0Addr),
    succ(Param0Ptr, Param1Ptr),
    readmem(Param1Ptr, Memory, Param1Addr),
    succ(Param1Ptr, Param2Ptr),
    readmem(Param2Ptr, Memory, Param2Addr),
    readmem(Param0Addr, Memory, Param0),
    readmem(Param1Addr, Memory, Param1),
    do_opcode(Opcode, Param0, Param1, OutVal, IPInc),
    writemem(Param2Addr, OutVal, Memory, FinalMem).

writemem(Addr, Val, Memory, UpdMemory) :-
    nth0(Addr, Memory, _, M1),
    nth0(Addr, UpdMemory, Val, M1).

readmem(Addr, Memory, Val) :-
    nth0(Addr, Memory, Val, _).

do_opcode(1, I0, I1, O, 4) :- plus(I0, I1, O).
do_opcode(2, I0, I1, O, 4) :- O is I0 * I1.

% ----------------------------------------------------------------------

test1() :-
    read_intcode(Program, "../../inputs/day2/testinp1"),
    read_intcode(Result, "../../inputs/day2/testout1"),
    run_program(Program, Final),
    Result == Final.

test2() :-
    read_intcode(Program, "../../inputs/day2/testinp2"),
    read_intcode(Result, "../../inputs/day2/testout2"),
    run_program(Program, Final),
    Result == Final.

test3() :-
    read_intcode(Program, "../../inputs/day2/testinp3"),
    read_intcode(Result, "../../inputs/day2/testout3"),
    run_program(Program, Final),
    Result == Final.

test4() :-
    read_intcode(Program, "../../inputs/day2/testinp4"),
    read_intcode(Result, "../../inputs/day2/testout4"),
    run_program(Program, Final),
    Result == Final.

test5() :-
    read_intcode(Program, "../../inputs/day2/testinp5"),
    read_intcode(Result, "../../inputs/day2/testout5"),
    run_program(Program, Final),
    Result == Final.

test() :- test1, test2, test3, test4, test5.

% ----------------------------------------------------------------------

program(FName, Noun, Verb, Result) :-
    read_intcode(Program, FName),
    between(0, 99, Noun),
    between(0, 99, Verb),
    writemem(1, Noun, Program, P1),
    writemem(2, Verb, P1, P2),
    run_program(P2, Final),
    readmem(0, Final, Result).
