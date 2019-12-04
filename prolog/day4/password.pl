password(Min, Max, Password) :-
    between(0, 9, D1),
    between(D1, 9, D2),
    between(D2, 9, D3),
    between(D3, 9, D4),
    between(D4, 9, D5),
    nth0(DupIdx, [D1, D2, D3, D4, D5], DD),
    nth0(DupIdx, PasswordList, DD, [D1, D2, D3, D4, D5]),
    intval(PasswordList, 6, Password),
    between(Min, Max, Password).

password2(Min, Max, Password) :-
    between(0, 9, D1),
    between(D1, 9, D2),
    between(D2, 9, D3),
    between(D3, 9, D4),
    between(D4, 9, D5),
    Inp = [D1, D2, D3, D4, D5],
    nth0(DupIdx, Inp, DD),
    
    succ(DupIdx, AfterDup),
    ((length(Inp, AfterDup)) ;
     \+ nth0(AfterDup, Inp, DD)),

    ((0 =:= DupIdx) ;
     (succ(BeforeDup, DupIdx),
      \+ nth0(BeforeDup, Inp, DD))),
    
    nth0(DupIdx, PasswordList, DD, Inp),
    intval(PasswordList, 6, Password),
    between(Min, Max, Password).

intval([], 0, 0).
intval([D|DS], ExpPlus, V) :-
    intval(DS, Exp, Low),
    High is ((10 ** Exp) * D),
    plus(High, Low, V),
    succ(Exp, ExpPlus).

test() :-
    password(000000, 999999, 111111),
    \+ password(000000, 999999, 223450),
    \+ password(000000, 999999, 123789),
    password2(000000, 999999, 112233),
    \+ password2(000000, 999999, 123444),
    password2(000000, 999999, 111122).
    

num_passwords_part1(Min, Max, Count) :-
    setof(P, password(Min, Max, P), PS), length(PS, Count).

num_passwords_part2(Min, Max, Count) :-
    setof(P, password2(Min, Max, P), PS), length(PS, Count).
