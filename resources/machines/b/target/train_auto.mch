MACHINE Train_1_beebook_TLC
SETS BLOCKS={A,B,C,D,E,F,G,H,I,J,K,L,M,N}; ROUTES={R1,R2,R3,R4,R5,R6,R7,R8,R9,R10}
CONSTANTS fst, lst, nxt, rtbl
VARIABLES LBT, TRK, frm, OCC, resbl, resrt, rsrtbl
PROPERTIES
dom(rtbl)=BLOCKS & ran(rtbl)=ROUTES & nxt:ROUTES-->(BLOCKS>+>BLOCKS) & fst:ROUTES-->BLOCKS & lst:ROUTES-->BLOCKS & fst~<:rtbl & lst~<:rtbl & !r.(r:ROUTES => fst(r)/=lst(r)) & !r.(r:ROUTES => !S.(S<:ran(nxt(r)) & S<:nxt(r)[S] => S={})) & !r.(r:ROUTES => nxt(r):rtbl~[{r}]\{lst(r)}>->>rtbl~[{r}]\{fst(r)}) & !r,s.(r:ROUTES & s:ROUTES & r/=s => fst(r)/:rtbl~[{s}]\{fst(s),lst(s)}) & !r,s.(r:ROUTES & s:ROUTES & r/=s => lst(r)/:rtbl~[{s}]\{fst(s),lst(s)}) & rtbl={b,r|b:BLOCKS & (r:dom(nxt) & (b:dom(nxt(r)) or b:ran(nxt(r))))} & nxt={(R6,{(B,A),(A,L),(C,B)}),(R4,{(K,F),(I,K),(H,I),(M,H),(F,G)}),(R7,{(B,A),(F,E),(D,B),(G,F),(E,D),(A,L)}),(R8,{(B,A),(K,D),(J,K),(D,B),(N,J),(A,L)}),(R10,{(J,I),(H,M),(I,H),(N,J)}),(R5,{(H,I),(M,H),(I,J),(J,N)}),(R9,{(F,K),(G,F),(H,M),(I,H),(K,I)}),(R1,{(B,C),(A,B),(L,A)}),(R2,{(F,G),(D,E),(E,F),(A,B),(B,D),(L,A)}),(R3,{(D,K),(K,J),(A,B),(B,D),(J,N),(L,A)})} & fst={(R1,L),(R8,N),(R9,G),(R2,L),(R6,C),(R3,L),(R7,G),(R10,N),(R4,M),(R5,M)} & lst={(R3,N),(R4,G),(R10,M),(R1,C),(R7,L),(R8,L),(R6,L),(R9,M),(R5,N),(R2,G)}
INVARIANT rsrtbl:resbl-->resrt & rsrtbl<:rtbl & OCC<:resbl & !r.(r:ROUTES => nxt(r)[rtbl~[{r}]\rsrtbl~[{r}]]/\rsrtbl~[{r}]\OCC={}) & !r.(r:ROUTES => nxt(r)[rsrtbl~[{r}]]<:rsrtbl~[{r}]) & !r.(r:ROUTES => nxt(r)[rsrtbl~[{r}]\OCC]<:rsrtbl~[{r}]\OCC) & TRK:BLOCKS>+>BLOCKS & frm<:resrt & rsrtbl[OCC]<:frm & !r.(r:resrt\frm => rtbl|>{r}=rsrtbl|>{r}) & !x,y.(x:BLOCKS & y:BLOCKS & (x,y):TRK => #r.(r:ROUTES & (x,y):nxt(r))) & !r.(r:frm => rsrtbl~[{r}]<|nxt(r)=rsrtbl~[{r}]<|TRK) & LBT<:OCC & !a,b.(b:LBT & (b:ran(nxt(rsrtbl(b))) & (a=nxt(rsrtbl(b))~(b) & a:dom(rsrtbl))) => rsrtbl(a)/=rsrtbl(b))
ASSERTIONS
!b.(b:OCC & b:dom(TRK) => nxt(rsrtbl(b))(b)=TRK(b)); ran(lst)/\dom(TRK)\ran(fst)={}; ran(fst)/\ran(TRK)\ran(lst)={}
INITIALISATION resrt := {} || resbl := {} || rsrtbl := {} || OCC := {} || TRK := {} || frm := {} || LBT := {}
OPERATIONS
route_reservationR1 = SELECT R1/:resrt & rtbl~[{R1}]/\resbl={} THEN resrt := resrt\/{R1} || rsrtbl := rsrtbl\/(rtbl|>{R1}) || resbl := resbl\/rtbl~[{R1}] END ;
route_reservationR10 = SELECT R10/:resrt & rtbl~[{R10}]/\resbl={} THEN resrt := resrt\/{R10} || rsrtbl := rsrtbl\/(rtbl|>{R10}) || resbl := resbl\/rtbl~[{R10}] END ;
route_reservationR2 = SELECT R2/:resrt & rtbl~[{R2}]/\resbl={} THEN resrt := resrt\/{R2} || rsrtbl := rsrtbl\/(rtbl|>{R2}) || resbl := resbl\/rtbl~[{R2}] END ;
route_reservationR3 = SELECT R3/:resrt & rtbl~[{R3}]/\resbl={} THEN resrt := resrt\/{R3} || rsrtbl := rsrtbl\/(rtbl|>{R3}) || resbl := resbl\/rtbl~[{R3}] END ;
route_reservationR4 = SELECT R4/:resrt & rtbl~[{R4}]/\resbl={} THEN resrt := resrt\/{R4} || rsrtbl := rsrtbl\/(rtbl|>{R4}) || resbl := resbl\/rtbl~[{R4}] END ;
route_reservationR5 = SELECT R5/:resrt & rtbl~[{R5}]/\resbl={} THEN resrt := resrt\/{R5} || rsrtbl := rsrtbl\/(rtbl|>{R5}) || resbl := resbl\/rtbl~[{R5}] END ;
route_reservationR6 = SELECT R6/:resrt & rtbl~[{R6}]/\resbl={} THEN resrt := resrt\/{R6} || rsrtbl := rsrtbl\/(rtbl|>{R6}) || resbl := resbl\/rtbl~[{R6}] END ;
route_reservationR7 = SELECT R7/:resrt & rtbl~[{R7}]/\resbl={} THEN resrt := resrt\/{R7} || rsrtbl := rsrtbl\/(rtbl|>{R7}) || resbl := resbl\/rtbl~[{R7}] END ;
route_reservationR8 = SELECT R8/:resrt & rtbl~[{R8}]/\resbl={} THEN resrt := resrt\/{R8} || rsrtbl := rsrtbl\/(rtbl|>{R8}) || resbl := resbl\/rtbl~[{R8}] END ;
route_reservationR9 = SELECT R9/:resrt & rtbl~[{R9}]/\resbl={} THEN resrt := resrt\/{R9} || rsrtbl := rsrtbl\/(rtbl|>{R9}) || resbl := resbl\/rtbl~[{R9}] END ;
route_freeingR1 = SELECT R1:resrt\ran(rsrtbl) THEN resrt := resrt\{R1} || frm := frm\{R1} END ;
route_freeingR10 = SELECT R10:resrt\ran(rsrtbl) THEN resrt := resrt\{R10} || frm := frm\{R10} END ;
route_freeingR2 = SELECT R2:resrt\ran(rsrtbl) THEN resrt := resrt\{R2} || frm := frm\{R2} END ;
route_freeingR3 = SELECT R3:resrt\ran(rsrtbl) THEN resrt := resrt\{R3} || frm := frm\{R3} END ;
route_freeingR4 = SELECT R4:resrt\ran(rsrtbl) THEN resrt := resrt\{R4} || frm := frm\{R4} END ;
route_freeingR5 = SELECT R5:resrt\ran(rsrtbl) THEN resrt := resrt\{R5} || frm := frm\{R5} END ;
route_freeingR6 = SELECT R6:resrt\ran(rsrtbl) THEN resrt := resrt\{R6} || frm := frm\{R6} END ;
route_freeingR7 = SELECT R7:resrt\ran(rsrtbl) THEN resrt := resrt\{R7} || frm := frm\{R7} END ;
route_freeingR8 = SELECT R8:resrt\ran(rsrtbl) THEN resrt := resrt\{R8} || frm := frm\{R8} END ;
route_freeingR9 = SELECT R9:resrt\ran(rsrtbl) THEN resrt := resrt\{R9} || frm := frm\{R9} END ;
FRONT_MOVE_1R1 = SELECT R1:frm & fst(R1):resbl\OCC & rsrtbl(fst(R1))=R1 THEN OCC := OCC\/{fst(R1)} || LBT := LBT\/{fst(R1)} END ;
FRONT_MOVE_1R10 = SELECT R10:frm & fst(R10):resbl\OCC & rsrtbl(fst(R10))=R10 THEN OCC := OCC\/{fst(R10)} || LBT := LBT\/{fst(R10)} END ;
FRONT_MOVE_1R2 = SELECT R2:frm & fst(R2):resbl\OCC & rsrtbl(fst(R2))=R2 THEN OCC := OCC\/{fst(R2)} || LBT := LBT\/{fst(R2)} END ;
FRONT_MOVE_1R3 = SELECT R3:frm & fst(R3):resbl\OCC & rsrtbl(fst(R3))=R3 THEN OCC := OCC\/{fst(R3)} || LBT := LBT\/{fst(R3)} END ;
FRONT_MOVE_1R4 = SELECT R4:frm & fst(R4):resbl\OCC & rsrtbl(fst(R4))=R4 THEN OCC := OCC\/{fst(R4)} || LBT := LBT\/{fst(R4)} END ;
FRONT_MOVE_1R5 = SELECT R5:frm & fst(R5):resbl\OCC & rsrtbl(fst(R5))=R5 THEN OCC := OCC\/{fst(R5)} || LBT := LBT\/{fst(R5)} END ;
FRONT_MOVE_1R6 = SELECT R6:frm & fst(R6):resbl\OCC & rsrtbl(fst(R6))=R6 THEN OCC := OCC\/{fst(R6)} || LBT := LBT\/{fst(R6)} END ;
FRONT_MOVE_1R7 = SELECT R7:frm & fst(R7):resbl\OCC & rsrtbl(fst(R7))=R7 THEN OCC := OCC\/{fst(R7)} || LBT := LBT\/{fst(R7)} END ;
FRONT_MOVE_1R8 = SELECT R8:frm & fst(R8):resbl\OCC & rsrtbl(fst(R8))=R8 THEN OCC := OCC\/{fst(R8)} || LBT := LBT\/{fst(R8)} END ;
FRONT_MOVE_1R9 = SELECT R9:frm & fst(R9):resbl\OCC & rsrtbl(fst(R9))=R9 THEN OCC := OCC\/{fst(R9)} || LBT := LBT\/{fst(R9)} END ;
FRONT_MOVE_2A = SELECT A:OCC & A:dom(TRK) & TRK(A)/:OCC THEN OCC := OCC\/{TRK(A)} END ;
FRONT_MOVE_2B = SELECT B:OCC & B:dom(TRK) & TRK(B)/:OCC THEN OCC := OCC\/{TRK(B)} END ;
FRONT_MOVE_2C = SELECT C:OCC & C:dom(TRK) & TRK(C)/:OCC THEN OCC := OCC\/{TRK(C)} END ;
FRONT_MOVE_2D = SELECT D:OCC & D:dom(TRK) & TRK(D)/:OCC THEN OCC := OCC\/{TRK(D)} END ;
FRONT_MOVE_2E = SELECT E:OCC & E:dom(TRK) & TRK(E)/:OCC THEN OCC := OCC\/{TRK(E)} END ;
FRONT_MOVE_2F = SELECT F:OCC & F:dom(TRK) & TRK(F)/:OCC THEN OCC := OCC\/{TRK(F)} END ;
FRONT_MOVE_2G = SELECT G:OCC & G:dom(TRK) & TRK(G)/:OCC THEN OCC := OCC\/{TRK(G)} END ;
FRONT_MOVE_2H = SELECT H:OCC & H:dom(TRK) & TRK(H)/:OCC THEN OCC := OCC\/{TRK(H)} END ;
FRONT_MOVE_2I = SELECT I:OCC & I:dom(TRK) & TRK(I)/:OCC THEN OCC := OCC\/{TRK(I)} END ;
FRONT_MOVE_2J = SELECT J:OCC & J:dom(TRK) & TRK(J)/:OCC THEN OCC := OCC\/{TRK(J)} END ;
FRONT_MOVE_2K = SELECT K:OCC & K:dom(TRK) & TRK(K)/:OCC THEN OCC := OCC\/{TRK(K)} END ;
FRONT_MOVE_2L = SELECT L:OCC & L:dom(TRK) & TRK(L)/:OCC THEN OCC := OCC\/{TRK(L)} END ;
FRONT_MOVE_2M = SELECT M:OCC & M:dom(TRK) & TRK(M)/:OCC THEN OCC := OCC\/{TRK(M)} END ;
FRONT_MOVE_2N = SELECT N:OCC & N:dom(TRK) & TRK(N)/:OCC THEN OCC := OCC\/{TRK(N)} END ;
BACK_MOVE_1A = SELECT A:LBT & A/:dom(TRK) THEN OCC := OCC\{A} || rsrtbl := {A}<<|rsrtbl || resbl := resbl\{A} || LBT := LBT\{A} END ;
BACK_MOVE_1B = SELECT B:LBT & B/:dom(TRK) THEN OCC := OCC\{B} || rsrtbl := {B}<<|rsrtbl || resbl := resbl\{B} || LBT := LBT\{B} END ;
BACK_MOVE_1C = SELECT C:LBT & C/:dom(TRK) THEN OCC := OCC\{C} || rsrtbl := {C}<<|rsrtbl || resbl := resbl\{C} || LBT := LBT\{C} END ;
BACK_MOVE_1D = SELECT D:LBT & D/:dom(TRK) THEN OCC := OCC\{D} || rsrtbl := {D}<<|rsrtbl || resbl := resbl\{D} || LBT := LBT\{D} END ;
BACK_MOVE_1E = SELECT E:LBT & E/:dom(TRK) THEN OCC := OCC\{E} || rsrtbl := {E}<<|rsrtbl || resbl := resbl\{E} || LBT := LBT\{E} END ;
BACK_MOVE_1F = SELECT F:LBT & F/:dom(TRK) THEN OCC := OCC\{F} || rsrtbl := {F}<<|rsrtbl || resbl := resbl\{F} || LBT := LBT\{F} END ;
BACK_MOVE_1G = SELECT G:LBT & G/:dom(TRK) THEN OCC := OCC\{G} || rsrtbl := {G}<<|rsrtbl || resbl := resbl\{G} || LBT := LBT\{G} END ;
BACK_MOVE_1H = SELECT H:LBT & H/:dom(TRK) THEN OCC := OCC\{H} || rsrtbl := {H}<<|rsrtbl || resbl := resbl\{H} || LBT := LBT\{H} END ;
BACK_MOVE_1I = SELECT I:LBT & I/:dom(TRK) THEN OCC := OCC\{I} || rsrtbl := {I}<<|rsrtbl || resbl := resbl\{I} || LBT := LBT\{I} END ;
BACK_MOVE_1J = SELECT J:LBT & J/:dom(TRK) THEN OCC := OCC\{J} || rsrtbl := {J}<<|rsrtbl || resbl := resbl\{J} || LBT := LBT\{J} END ;
BACK_MOVE_1K = SELECT K:LBT & K/:dom(TRK) THEN OCC := OCC\{K} || rsrtbl := {K}<<|rsrtbl || resbl := resbl\{K} || LBT := LBT\{K} END ;
BACK_MOVE_1L = SELECT L:LBT & L/:dom(TRK) THEN OCC := OCC\{L} || rsrtbl := {L}<<|rsrtbl || resbl := resbl\{L} || LBT := LBT\{L} END ;
BACK_MOVE_1M = SELECT M:LBT & M/:dom(TRK) THEN OCC := OCC\{M} || rsrtbl := {M}<<|rsrtbl || resbl := resbl\{M} || LBT := LBT\{M} END ;
BACK_MOVE_1N = SELECT N:LBT & N/:dom(TRK) THEN OCC := OCC\{N} || rsrtbl := {N}<<|rsrtbl || resbl := resbl\{N} || LBT := LBT\{N} END ;
BACK_MOVE_2A = SELECT A:LBT & A:dom(TRK) & TRK(A):OCC THEN OCC := OCC\{A} || rsrtbl := {A}<<|rsrtbl || resbl := resbl\{A} || LBT := LBT\{A}\/{TRK(A)} END ;
BACK_MOVE_2B = SELECT B:LBT & B:dom(TRK) & TRK(B):OCC THEN OCC := OCC\{B} || rsrtbl := {B}<<|rsrtbl || resbl := resbl\{B} || LBT := LBT\{B}\/{TRK(B)} END ;
BACK_MOVE_2C = SELECT C:LBT & C:dom(TRK) & TRK(C):OCC THEN OCC := OCC\{C} || rsrtbl := {C}<<|rsrtbl || resbl := resbl\{C} || LBT := LBT\{C}\/{TRK(C)} END ;
BACK_MOVE_2D = SELECT D:LBT & D:dom(TRK) & TRK(D):OCC THEN OCC := OCC\{D} || rsrtbl := {D}<<|rsrtbl || resbl := resbl\{D} || LBT := LBT\{D}\/{TRK(D)} END ;
BACK_MOVE_2E = SELECT E:LBT & E:dom(TRK) & TRK(E):OCC THEN OCC := OCC\{E} || rsrtbl := {E}<<|rsrtbl || resbl := resbl\{E} || LBT := LBT\{E}\/{TRK(E)} END ;
BACK_MOVE_2F = SELECT F:LBT & F:dom(TRK) & TRK(F):OCC THEN OCC := OCC\{F} || rsrtbl := {F}<<|rsrtbl || resbl := resbl\{F} || LBT := LBT\{F}\/{TRK(F)} END ;
BACK_MOVE_2G = SELECT G:LBT & G:dom(TRK) & TRK(G):OCC THEN OCC := OCC\{G} || rsrtbl := {G}<<|rsrtbl || resbl := resbl\{G} || LBT := LBT\{G}\/{TRK(G)} END ;
BACK_MOVE_2H = SELECT H:LBT & H:dom(TRK) & TRK(H):OCC THEN OCC := OCC\{H} || rsrtbl := {H}<<|rsrtbl || resbl := resbl\{H} || LBT := LBT\{H}\/{TRK(H)} END ;
BACK_MOVE_2I = SELECT I:LBT & I:dom(TRK) & TRK(I):OCC THEN OCC := OCC\{I} || rsrtbl := {I}<<|rsrtbl || resbl := resbl\{I} || LBT := LBT\{I}\/{TRK(I)} END ;
BACK_MOVE_2J = SELECT J:LBT & J:dom(TRK) & TRK(J):OCC THEN OCC := OCC\{J} || rsrtbl := {J}<<|rsrtbl || resbl := resbl\{J} || LBT := LBT\{J}\/{TRK(J)} END ;
BACK_MOVE_2K = SELECT K:LBT & K:dom(TRK) & TRK(K):OCC THEN OCC := OCC\{K} || rsrtbl := {K}<<|rsrtbl || resbl := resbl\{K} || LBT := LBT\{K}\/{TRK(K)} END ;
BACK_MOVE_2L = SELECT L:LBT & L:dom(TRK) & TRK(L):OCC THEN OCC := OCC\{L} || rsrtbl := {L}<<|rsrtbl || resbl := resbl\{L} || LBT := LBT\{L}\/{TRK(L)} END ;
BACK_MOVE_2M = SELECT M:LBT & M:dom(TRK) & TRK(M):OCC THEN OCC := OCC\{M} || rsrtbl := {M}<<|rsrtbl || resbl := resbl\{M} || LBT := LBT\{M}\/{TRK(M)} END ;
BACK_MOVE_2N = SELECT N:LBT & N:dom(TRK) & TRK(N):OCC THEN OCC := OCC\{N} || rsrtbl := {N}<<|rsrtbl || resbl := resbl\{N} || LBT := LBT\{N}\/{TRK(N)} END ;
point_positionningR1 = SELECT R1:resrt\frm THEN TRK := dom(nxt(R1))<<|TRK|>>ran(nxt(R1))\/nxt(R1) END ;
point_positionningR10 = SELECT R10:resrt\frm THEN TRK := dom(nxt(R10))<<|TRK|>>ran(nxt(R10))\/nxt(R10) END ;
point_positionningR2 = SELECT R2:resrt\frm THEN TRK := dom(nxt(R2))<<|TRK|>>ran(nxt(R2))\/nxt(R2) END ;
point_positionningR3 = SELECT R3:resrt\frm THEN TRK := dom(nxt(R3))<<|TRK|>>ran(nxt(R3))\/nxt(R3) END ;
point_positionningR4 = SELECT R4:resrt\frm THEN TRK := dom(nxt(R4))<<|TRK|>>ran(nxt(R4))\/nxt(R4) END ;
point_positionningR5 = SELECT R5:resrt\frm THEN TRK := dom(nxt(R5))<<|TRK|>>ran(nxt(R5))\/nxt(R5) END ;
point_positionningR6 = SELECT R6:resrt\frm THEN TRK := dom(nxt(R6))<<|TRK|>>ran(nxt(R6))\/nxt(R6) END ;
point_positionningR7 = SELECT R7:resrt\frm THEN TRK := dom(nxt(R7))<<|TRK|>>ran(nxt(R7))\/nxt(R7) END ;
point_positionningR8 = SELECT R8:resrt\frm THEN TRK := dom(nxt(R8))<<|TRK|>>ran(nxt(R8))\/nxt(R8) END ;
point_positionningR9 = SELECT R9:resrt\frm THEN TRK := dom(nxt(R9))<<|TRK|>>ran(nxt(R9))\/nxt(R9) END ;
route_formationR1 = SELECT R1:resrt\frm & rsrtbl~[{R1}]<|nxt(R1)=rsrtbl~[{R1}]<|TRK THEN frm := frm\/{R1} END ;
route_formationR10 = SELECT R10:resrt\frm & rsrtbl~[{R10}]<|nxt(R10)=rsrtbl~[{R10}]<|TRK THEN frm := frm\/{R10} END ;
route_formationR2 = SELECT R2:resrt\frm & rsrtbl~[{R2}]<|nxt(R2)=rsrtbl~[{R2}]<|TRK THEN frm := frm\/{R2} END ;
route_formationR3 = SELECT R3:resrt\frm & rsrtbl~[{R3}]<|nxt(R3)=rsrtbl~[{R3}]<|TRK THEN frm := frm\/{R3} END ;
route_formationR4 = SELECT R4:resrt\frm & rsrtbl~[{R4}]<|nxt(R4)=rsrtbl~[{R4}]<|TRK THEN frm := frm\/{R4} END ;
route_formationR5 = SELECT R5:resrt\frm & rsrtbl~[{R5}]<|nxt(R5)=rsrtbl~[{R5}]<|TRK THEN frm := frm\/{R5} END ;
route_formationR6 = SELECT R6:resrt\frm & rsrtbl~[{R6}]<|nxt(R6)=rsrtbl~[{R6}]<|TRK THEN frm := frm\/{R6} END ;
route_formationR7 = SELECT R7:resrt\frm & rsrtbl~[{R7}]<|nxt(R7)=rsrtbl~[{R7}]<|TRK THEN frm := frm\/{R7} END ;
route_formationR8 = SELECT R8:resrt\frm & rsrtbl~[{R8}]<|nxt(R8)=rsrtbl~[{R8}]<|TRK THEN frm := frm\/{R8} END ;
route_formationR9 = SELECT R9:resrt\frm & rsrtbl~[{R9}]<|nxt(R9)=rsrtbl~[{R9}]<|TRK THEN frm := frm\/{R9} END 
END