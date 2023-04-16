%------------------------------GENERAL--------------------------------------------------------
convertBinToDec(Bin,Dec) :- 
                            
                            atom_chars(Bin,BinL),
                            length(BinL,N),
                            N1 is N - 1,
                            convertBinToDec(BinL,Dec,N1).

convertBinToDec([],0,_).
convertBinToDec(['0'|T],Dec,I) :-

    I1 is I - 1,
    convertBinToDec(T,Dec,I1).

convertBinToDec(['1'|T],Dec,I) :-

    I1 is I - 1,

    convertBinToDec(T,Dec1,I1),
    Dec is Dec1 + 2 ** I.




replaceIthItem(Item,[_|TL],0,[Item|TL]).
replaceIthItem(Item,[HL|TL],I,[HL|TR]) :-

    I > 0,
    I1 is I - 1,
    replaceIthItem(Item,TL,I1,TR).

splitEvery(N,List,Res) :- splitEvery(N,List,Res,0,[]).

splitEvery(_,[],[Acc],_,Acc).

splitEvery(N,L,[Acc|T],N,Acc):-
    N > 0,
    L \= [],
    splitEvery(N,L,T,0,[]).

splitEvery(N,[H|T],Res,C,Acc) :-

    C < N,
    C1 is C + 1,
    append(Acc,[H],Acc1),
    splitEvery(N,T,Res,C1,Acc1).


    
logBase2(X,N) :-

    N is round(log(X) / log(2)).




getNumBits(_,fullyAssoc,_,0).

getNumBits(_,directMap,Cache,BitsNum) :-
    length(Cache,N),

    logBase2(N,BitsNum).

getNumBits(SetNum,setAssoc,Cache,BitsNum) :-
    length(Cache,N),

    logBase2(N,BitsNum1),
    BitsNum is BitsNum1 // SetNum + (BitsNum1 mod SetNum).




fillZeros(String,0,String).
fillZeros(String,N,R) :-

    N > 0,
    N1 is N - 1,
    string_concat("0",String,String1),
    fillZeros(String1,N1,R).

% ------------------------------GENERAL---------------------------------------------------

% ------------------------------DIRECT MAPPING-----------------------------------------------


 getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum) :-
    
    atom_number(StringAddress,NumberAddress),
    convertAddress(NumberAddress,BitsNum,Tag,Idx,directMap),
    convertBinToDec(Tag,HopsNum),
    convertBinToDec(Idx,IdxD),
    string_length(StringAddress,N),
    atom_length(Tag,TagN),
    F is N - BitsNum - TagN,

    fillZeros(Tag,F,TagF),
    getDataFromCache(TagF,IdxD,Cache,Data,0).



getDataFromCache(Tag,Idx,[item(tag(Tag),data(Data),1,_)|_],Data,Idx).

getDataFromCache(Tag,Idx,[item(tag(Tag1),_,_,_)|T],Data,C) :-
    Idx \= C,
    C1 is C + 1,
    getDataFromCache(Tag,Idx,T,Data,C1).








convertAddress(Bin,BitsNum,Tag,Idx,directMap) :- convertAddress(Bin,BitsNum,Tag,Idx,0,directMap).


convertAddress(Bin,BitsNum,Bin,0,BitsNum,directMap).
convertAddress(Bin,BitsNum,Tag,Idx,C,directMap):-

    C < BitsNum,
    R is Bin mod 10,
    D is Bin // 10,
    C1 is C+1,
    convertAddress(D,BitsNum,Tag,Idx1,C1,directMap),
    Idx is Idx1 + R * (10 ** C).
    

replaceInCache(Tag,Idx,Mem,[item(tag(Tag1),D,I,O)|T],NewCache,ItemData,directMap,BitsNum):-
    
    atom_length(Idx,IdxN),
    atom_length(Tag,TagN),
    atom_length(Tag1,Tag1N),
    IF is BitsNum - IdxN,
    fillZeros(Idx,IF,IdxF),
    
    TF is Tag1N - TagN,
    fillZeros(Tag,TF,TagF),
    convertBinToDec(Idx,IdxD),
    string_concat(TagF,IdxF,AddressBin),
    convertBinToDec(AddressBin,AddressDec),
    nth0(AddressDec,Mem,ItemData),
    replaceInCacheHelper(TagF,IdxD,[item(tag(Tag1),D,I,O)|T],NewCache,ItemData,directMap).

replaceInCacheHelper(Tag,0,[_|TOC],[item(tag(Tag), data(ItemData), 1, 0)|TOC],ItemData,directMap).

replaceInCacheHelper(Tag,Idx,[HOC|TOC],[HOC|TNC],ItemData,directMap) :-

    Idx > 0,
    Idx1 is Idx - 1,
    replaceInCacheHelper(Tag,Idx1,TOC,TNC,ItemData,directMap).


% ------------------------------DIRECT MAPPING-----------------------------------------------

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit) :-
    getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
    NewCache = OldCache.



getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss) :-

    \+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
    atom_number(StringAddress,Address),
    convertAddress(Address,BitsNum,Tag,Idx,Type),
    replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).

runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets) :-
    
    getNumBits(NumOfSets,Type,OldCache,BitsNum),
    (
        Type = setAssoc, Num = NumOfSets; 
        Type \= setAssoc, Num = BitsNum
    ),
    getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
    runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).
