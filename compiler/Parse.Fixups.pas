unit Parse.Fixups;

interface

//Performs branch fixups.
//Branch fixups are needed after multiple execution paths have been parsed.
//Each variable stores the current assign version index. If the variable has
//been assigned a value in an earlier (alternate) code branch then it's Version
//will be incorrect on entry to the second path. This routine resolves that issue,
//and needs to be called once the branches merge.
//LeftIndex and LeftStopIndex are indexes into the IL list of the first and last items of
//    the (second, ELSE) code path
//RightFirstIndex and RightStopIndex are the indexes into the IL list of the first
//    and last items in the other code path (THEN clause)
procedure BranchFixup(LeftIndex, LeftStopIndex, RightIndex, RightStopIndex: Integer);

//Sub to BranchFixup for anyone brave enough :)
procedure BranchFixupRight(Index, StopIndex: Integer);

//Walk the IL data and append phi nodes
//Phi nodes are used where paths merge and we need to establish which varsions
//of variables are needed for different paths. For each path the phi node stores the
//variable version and the block which branches to it.
//Must only be called at the start of a Block, and where multiple paths merge
//Path1Index is the Index of the last IL item in the first path
//Path2Index is the Index of the last IL item in the second path, or -1 if there
//    is no second path (e.g. an IF with no ELSE) - where we only need compare against
//    the 'origin' (see below)
//OriginIndex. The 'origin' is the point before the paths split. I.e. the IF statement
//Path1BlockID is the ID of the last Block in the first path
//Path2BlockID is the ID of the last Block in the second path. If there is no second path
//    (i.e only the 'origin' path) this should be the ID of the last Block in the
//    origin path (i.e. that of the IF statement)
//NOTE: OriginIndex MUST IMMEDIATLY PRECEDE first item in Path1.
//      Path1Index MUST IMMEDIATELY PRECEDE first item in Path2.
//      (i.e. OriginIndex and Path1Index are used as the terminating points for the walks
//      of Path1 and Path2 respectively).
procedure PhiWalk(Path1Index, Path2Index, OriginIndex: Integer;
  Path1BlockID, Path2BlockID: Integer);

//Internal version of PhiWalk for those who want to roll their own :)
//(The main use case is to be able to Touch a variable before calling so
//it is not processed. Be sure to call VarClearTouches to prepare other variables).

//Internal version of PhiWalk
//Walk backwards along a path,
//for each variable assignment (which has not already been encountered)
//  * Walks the second path to identify the last assigment
//    (or, if not found in the second path, walks the origin path to identify the same)
//  * Appends a phi node
//  * Sets the AdjustVersionFrom and AdjustVersionTo fields of the variable ready for BranchFixupRight
//    (after inserting phi nodes BranchFixupRight will be needed on the following code)
//
//Parameters are as PhiWalk except:
//StopIndex is the Index of the IL item preceding the first item in Path1
//If we are only walking one path (see Swap), Path2Index should be -1.
//Swap is True if we are walking the second path. If so the search will only compare
//  Path1 versus Origin.
//InsertIndex: If this is -1 new Phi nodes will be appended to the end of the IL list,
//  otherwise new nodes will be inserted at this Index.
//Returns a count of the number of nodes (IL Items) inserted
//NOTE: Here (versus PhiWalk) the terminating point of Path1 is explicitly given (by StopIndex)
//      therefore the requirement for Origin to immediately precede Path1 is removed.
//      However, of two paths are given the requirement for Path1 to immediately precede Path2 remains.
function PhiWalkInt(Path1Index, StopIndex, Path2Index, OriginIndex: Integer;
  Path1BlockID, Path2BlockID: Integer;Swap: Boolean;InsertIndex: Integer): Integer;


implementation
uses SysUtils,
  Def.IL, Def.Operators, Def.Variables;

//Scan all the IL items between Index and StopIndex inclusive
//For the first assignment to each variable, record (Version-1) to
//AdjustVersionTo of the variables data.
//Returns a count of the variable assignments found
function BranchFixupLeft(Index, StopIndex: Integer): Integer;

  procedure Fixup(var Param: TILParam);
  var Variable: PVariable;
  begin
    if Param.Kind = pkVarDest then
      begin
        Variable := Param.Variable;
        if Variable.AdjustVersionTo = -1 then
        begin
          Variable.AdjustVersionTo := Param.VarVersion - 1;
          inc(Result);
        end;
      end;
  end;

var ILItem: PILItem;
begin
  Result := 0;
  while Index <= StopIndex do
  begin
    ILItem := ILIndexToData(Index);
    Fixup(ILItem.Param1);
    Fixup(ILItem.Param2);
    Fixup(ILItem.Param3);
    inc(Index);
  end;
end;

//Scan the IL items between Index and StopIndex inclusive
//For each variable assignment: if it's the first to that variable, set the
//AdjustVersionFrom for that var to the Version of the assignment.
//For each read of a var (i.e as a parameter), adjust the Version if it's to
//a version of the var prior to the path. I.e. if it's Version is < AdjustVersionFrom,
//or if AdjustVersionFrom has yet to be assigned a value.
procedure BranchFixupRight(Index, StopIndex: Integer);

  procedure Fixup(var Param, PhiDest: TILParam);
  var Variable: PVariable;
  begin
    case Param.Kind of
      pkVarSource:
      begin
        Variable := Param.Variable;
        if Variable.AdjustVersionTo >= 0 then
          if (Variable.AdjustVersionFrom = -1) or (Param.VarVersion = Variable.AdjustVersionFrom) then
            Param.VarVersion := Variable.AdjustVersionTo;
      end;
      pkPhiVarSource:
      begin
        Variable := PhiDest.PhiVar;
        if Variable.AdjustVersionTo >= 0 then
          if (Variable.AdjustVersionFrom = -1) or (Param.PhiSourceVersion = Variable.AdjustVersionFrom) then
            Param.PhiSourceVersion := Variable.AdjustVersionTo;
      end;
      pkVarDest:
      begin
        Variable := Param.Variable;
        if Variable.AdjustVersionTo >= 0 then
          if Variable.AdjustVersionFrom = -1 then
            Variable.AdjustVersionFrom := Param.VarVersion - 1;
      end;
    end;
  end;

var ILItem: PILItem;
begin
  while Index <= StopIndex do
  begin
    ILItem := ILIndexToData(Index);
    Fixup(ILItem.Param1, ILItem.Dest);
    Fixup(ILItem.Param2, ILItem.Dest);
    Fixup(ILItem.Param3, ILItem.Dest);
    inc(Index);
  end;
end;

procedure BranchFixup(LeftIndex, LeftStopIndex, RightIndex, RightStopIndex: Integer);
begin
  Vars.ClearAdjust;

  //Short circuit if no assignments in the other path.
  if BranchFixupLeft(LeftIndex, LeftStopIndex) > 0 then
    BranchFixupRight(RightIndex, RightStopIndex);
end;


//Sub to PhiWalk. Walks backwards along a Path to find the first assignment to the
//given variable and returns the Version version of that assignment
//If not found returns zero
//VarIndex the Index of the variable to find
//Index is the IL item to begin walking from
//StopIndex is the IL item to finish walking at (last item of prior block)
function PhiFindVar(AVariable: PVariable;Index, StopIndex: Integer): Integer;
var ILItem: PILItem;
begin
  //Search up block
  while Index > StopIndex do
  begin
    //If assignment to var found
    ILItem := ILIndexToData(Index);
    if (ILItem.Param1.Kind = pkVarDest) and (ILItem.Param1.Variable = AVariable) then
      EXIT(ILItem.Param1.VarVersion);
    if (ILItem.Param2.Kind = pkVarDest) and (ILItem.Param2.Variable = AVariable) then
      EXIT(ILItem.Param2.VarVersion);
    if (ILItem.Param3.Kind = pkVarDest) and (ILItem.Param3.Variable = AVariable) then
      EXIT(ILItem.Param3.VarVersion);
    dec(Index);
  end;

  Result := 0;
end;

function PhiWalkInt(Path1Index, StopIndex, Path2Index, OriginIndex: Integer;
  Path1BlockID, Path2BlockID: Integer;Swap: Boolean;InsertIndex: Integer): Integer;

  procedure Process(const Param: TILParam;var Index: Integer);
  var Variable: PVariable;
    Path2Version: Integer;
    ILPhi: PILItem;
  begin
    if Param.Kind = pkVarDest then
    begin
      Variable := Param.Variable;
      if not Variable.Touched then
      begin
        Path2Version := 0;

        //If Path2, Search Path2
        if Path2Index > 0 then
          Path2Version := PhiFindVar(Param.Variable, Path2Index, Path1Index);

        //If No Path 2 or Not found in Path 1
        //Search Origin Path
        if Path2Version = 0 then
          Path2Version := PhiFindVar(Param.Variable, OriginIndex, -1);

        //If Path2Version is still zero then it was not assigned in either the other
        //path or the origin path. We'll stick with Version=0 to show this
        //(either a warning or a chance to auto-initialise tha variable)
        if InsertIndex = -1 then
          ILPhi := ILAppend(OpPhi)
        else
        begin
          ILPhi := ILInsert(InsertIndex, OpPhi);
          if Index > InsertIndex then
            inc(Index);
          if Path2Index > InsertIndex then
            inc(Path2Index);
          if OriginIndex > InsertIndex then
            inc(OriginIndex);
        end;

        ILPhi.Dest.SetPhiVarDest(Param.Variable, Variable.IncVersion);

        case Param.Kind of
          pkVarDest:
          begin
            ILPhi.Param1.Kind := pkPhiVarSource;
            ILPhi.Param1.PhiBlockID := Path1BlockID;

            ILPhi.Param2.Kind := pkPhiVarSource;
            ILPhi.Param2.PhiBlockID := Path2BlockID;

            if not Swap then
            begin
              ILPhi.Param1.PhiSourceVersion := Param.VarVersion;
              ILPhi.Param2.PhiSourceVersion := Path2Version;
            end
            else
            begin
              ILPhi.Param1.PhiSourceVersion := Path2Version;
              ILPhi.Param2.PhiSourceVersion := Param.VarVersion;
            end;
          end;
        else
          raise Exception.Create('Uncoded DestLoc type');
        end;

        //When inserting nodes fixups will be needed to any variable references after
        //the point where the nodes have been inserted. Used (eg) by FOR loops.
        Variable.AdjustVersionFrom := Path2Version;
        Variable.AdjustVersionTo := ILPhi.Dest.PhiDestVersion;

        inc(Result);  //Inc count

        Variable.Touch;
      end;
    end;
  end;

var
  Index: Integer;
  ILItem: PILItem;
begin
  Result := 0;  //Added count

  //Search up path 1
  Index := Path1Index;
  while Index > StopIndex do
  begin
    //For each assignment to an untouched var
    ILItem := ILIndexToData(Index);
    Process(ILItem.Param1, Index);
    Process(ILItem.Param2, Index);
    Process(ILItem.Param3, Index);

    dec(Index);
  end;
end;

procedure PhiWalk(Path1Index, Path2Index, OriginIndex: Integer;Path1BlockID, Path2BlockID: Integer);
begin
  Vars.ClearTouches;

  //Walk Path 1 and find related assignments in Path2 and Origin
  PhiWalkInt(Path1Index, OriginIndex, Path2Index, OriginIndex, Path1BlockID, Path2BlockID, False, -1);

  //For any assigments in Path 2 but not in Path 1,
  //Walk path 2 and find related assignments in the Origin path
  if Path2Index >= 0 then
    PhiWalkInt(Path2Index, Path1Index, -1, OriginIndex, Path1BlockID, Path2BlockID, True, -1);
end;

end.
