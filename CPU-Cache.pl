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

getDataFromCache(Tag,Idx,[_|T],Data,C) :-
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



% ------------------------------FULLY ASSOCIATIVE-----------------------------------------------


getDataFromCache(StringAddress,[item(tag(StringAddress1),data(Data),1,_)|_],Data,0,fullyAssoc,_):-
    atom_number(StringAddress,StringAddressI),
    atom_number(StringAddress1,StringAddressI).

getDataFromCache(StringAddress,[item(tag(StringAddress1),_,_,_)|T],Data,HopsNum,fullyAssoc,_):-
    atom_number(StringAddress,StringAddressI),
    atom_number(StringAddress1,StringAddressI1),
    StringAddressI \= StringAddressI1,
    getDataFromCache(StringAddress,T,Data,HopsNum1,fullyAssoc,_),
    HopsNum is HopsNum1 + 1.



convertAddress(Bin,_,Bin,_,fullyAssoc).


replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :- 

    if_invalid(OldCache),
    convertBinToDec(Tag,AddressDec),
    nth0(AddressDec,Mem,ItemData),
    replaceInCacheInInvalid(Tag,OldCache,NewCache1,ItemData),
    adjust_orders(NewCache1,ItemData,NewCache).

replaceInCache(Tag,_,Mem,[item(Ta,D,I,O)|T],NewCache,ItemData,fullyAssoc,_) :- 

    \+if_invalid([item(Ta,D,I,O)|T]),
    convertBinToDec(Tag,AddressDec),
    nth0(AddressDec,Mem,ItemData),
    get_min(T,Min,O),
    replaceInCacheInMin(Tag,[item(Ta,D,I,O)|T],NewCache1,ItemData,Min),
    adjust_orders(NewCache1,ItemData,NewCache).



replaceInCacheInInvalid(Tag,[item(tag(Tag1),_,0,_)|T],[item(tag(TagF),data(ItemData),1,0)|T],ItemData) :- 
    atom_length(Tag,TagN),
    atom_length(Tag1,Tag1N),
    TF is Tag1N - TagN,
    fillZeros(Tag,TF,TagF).

replaceInCacheInInvalid(Tag,[item(Ta,D,1,O)|TOC],[item(Ta,D,1,O)|TNC],ItemData) :-

    replaceInCacheInInvalid(Tag,TOC,TNC,ItemData).


replaceInCacheInMin(Tag,[item(tag(Tag1),_,_,Min)|T],[item(tag(TagF),data(ItemData),1,0)|T],ItemData,Min) :-

    atom_length(Tag,TagN),
    atom_length(Tag1,Tag1N),
    TF is Tag1N - TagN,
    fillZeros(Tag,TF,TagF).

replaceInCacheInMin(Tag,[item(Ta,D,I,O)|TOC],[item(Ta,D,I,O)|TNC],ItemData,Min) :-

    Min \= O,
    replaceInCacheInMin(Tag,TOC,TNC,ItemData,Min).





if_invalid([item(_,_,0,_)|_]).
if_invalid([item(_,_,1,_)|T]) :- if_invalid(T).

% Supposed to be max not min

get_min([],Min,Min).
get_min([item(_,_,_,O)|T],Min,MinSoFar) :-

    O > MinSoFar,
    get_min(T,Min,O).
get_min([item(_,_,_,O)|T],Min,MinSoFar) :-

    O =< MinSoFar,
    get_min(T,Min,MinSoFar).


adjust_orders([],_,[]).
adjust_orders([item(Ta,data(ItemData),1,O)|TC],ItemData,[item(Ta,data(ItemData),1,O)|TNC]) :-

    adjust_orders(TC,ItemData,TNC).

adjust_orders([item(Ta,data(Data),1,O)|TC],ItemData,[item(Ta,data(Data),1,O1)|TNC]) :-

    Data \= ItemData,
    O1 is O + 1,
    adjust_orders(TC,ItemData,TNC).
adjust_orders([item(Ta,data(Data),0,O)|TC],ItemData,[item(Ta,data(Data),0,O)|TNC]) :-

    Data \= ItemData,
    
    adjust_orders(TC,ItemData,TNC).




% ------------------------------FULLY ASSOCIATIVE-----------------------------------------------

% ------------------------------SET ASSOCIATIVE-----------------------------------------------

getDataFromCache(StringAddress,OldCache,Data,AddressDec,setAssoc,SetsNum) :-

    atom_number(StringAddress,AddressBin),
    convertBinToDec(AddressBin,AddressDec),
    
    convertAddress(AddressBin,SetsNum,Tag,Idx,setAssoc),
    convertBinToDec(Idx,IdxD),

    length(OldCache,OldCacheN),
    BlockSize is OldCacheN // SetsNum,
    (
        (
            IdxD mod BlockSize =:= 1,
            StoppingIndex is (IdxD // BlockSize) + (IdxD mod BlockSize) + 1
        );
        (
            IdxD mod BlockSize =:= 0,
            StoppingIndex is (IdxD // BlockSize) + (IdxD mod BlockSize)
        )
    ),
    get_subset(OldCache,SubSet,_,_,StoppingIndex,BlockSize),
    get_data_from_subset(SubSet,Tag,Data).

get_data_from_subset([item(tag(TagF),data(Data),1,_)|_],Tag,Data) :-

    atom_number(TagF,Tag).


get_data_from_subset([item(tag(TagF),data(ItemData),1,_)|T],Tag,Data) :-

    ItemData \= Data,
    get_data_from_subset(T,Tag,Data).










convertAddress(Bin,SetsNum,Tag,Idx,setAssoc) :-

    logBase2(SetsNum,BitsNum),
    convertAddress(Bin,BitsNum,Tag,Idx,directMap).

replaceInCache(Tag,Idx,Mem,[item(tag(Tag1),D,I,O)|T],NewCache,ItemData,setAssoc,SetsNum) :-

    
    
    atom_length(Idx,IdxN),
    atom_length(Tag,TagN),
    atom_length(Tag1,Tag1N),
    logBase2(SetsNum,BitsNum),
    IF is BitsNum - IdxN,
    fillZeros(Idx,IF,IdxF),
    
    TF is Tag1N - TagN,
    fillZeros(Tag,TF,TagF),

    convertBinToDec(Idx,IdxD),
    string_concat(TagF,IdxF,AddressBin),
    convertBinToDec(AddressBin,AddressDec),
    atom_number(AddressBin,AddressBinN),
    nth0(AddressDec,Mem,ItemData),

    length([item(tag(Tag1),D,I,O)|T],OldCacheN),
    BlockSize is OldCacheN // SetsNum,
    (
        (
            IdxD mod BlockSize =:= 1,
            StoppingIndex is (IdxD // BlockSize) + (IdxD mod BlockSize) + 1
        );
        (
            IdxD mod BlockSize =:= 0,
            StoppingIndex is (IdxD // BlockSize) + (IdxD mod BlockSize)
        )
    ),

    get_subset([item(tag(Tag1),D,I,O)|T],SubSet,Before,After,StoppingIndex,BlockSize),

    replaceInCache(AddressBinN,Idx,Mem,SubSet,NewSubCache,ItemData,fullyAssoc,BitsNum),

    append(Before,NewSubCache,NewCache1),
    append(NewCache1,After,NewCache).

get_subset(Cache,SubSet,[],After,0,BS) :-
    get_block(Cache,SubSet,After,BS).    

get_subset([H|T],SubSet,[H|BT],After,Idx,BS) :-
    Idx > 0,
    Idx1 is Idx - 1,
    get_subset(T,SubSet,BT,After,Idx1,BS).


get_block(Cache,[],Cache,0).
get_block([H|T],[H|ST],After,BS) :-

    BS > 0,
    BS1 is BS - 1,
    get_block(T,ST,After,BS1).

% ------------------------------SET ASSOCIATIVE-----------------------------------------------





% ------------------------------MAIN----------------------------------------------------------


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

% ------------------------------MAIN----------------------------------------------------------
