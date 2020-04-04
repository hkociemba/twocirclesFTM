unit twoPentacircles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  NPIECE = 18; // Anzahl der puzzle teile in den zwei RIngen
  BWSEARCHLIM: array [3 .. 9] of integer = (13, 13, 13, 13, 10, 13, 13);

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TTileID = array [0 .. NPIECE - 1] of integer;
  TTileOri = array [0 .. NPIECE - 1] of integer;
  TPuzOri = array [0 .. NPIECE - 1] of integer;

  TPuz = record
    id: TTileID;
    ori: TTileOri;
    pzo: TPuzOri;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses math;

var
  // pruningNCircle: array [0 .. 48620 - 1] of array of UInt64; // 48620 = 18 choose 9

  pruningNCircle: array of array of UInt64; // 48620 = 18 choose 9
  sofar: array [0 .. 50] of integer;
  solved: Boolean;
  NPOS: integer; // Anzahl der Positionen des pruning sets
  NPERM: integer; // Anzahl der Permutationen des pruning sets
  PRUNID, NSTATES: Int64;

function nfac(n: integer): integer;
begin
  if n = 1 then
    exit(1)
  else
    exit(n * nfac(n - 1));
end;

function c_nk(n, k: integer): integer;
var
  i, j: integer;
begin
  if n < k then
    exit(0);
  if k > n div 2 then
    k := n - k;
  result := 1;
  i := n;
  j := 1;
  while i <> n - k do
  begin
    result := result * i;
    result := result div j;
    Dec(i);
    inc(j);
  end;
end;

procedure rot_right(var a: Array of integer; l, r: integer);
// Rotate array arr right between l and r. r is included.
var
  tmp, i: integer;
begin
  tmp := a[r];
  for i := r downto l + 1 do
    a[i] := a[i - 1];
  a[l] := tmp
end;

procedure rot_left(var a: Array of integer; l, r: integer);
// Rotate array arr left between l and r. r is included.
var
  tmp, i: integer;
begin
  tmp := a[l];
  for i := l to r - 1 do
    a[i] := a[i + 1];
  a[r] := tmp
end;

function get_Ntupel_sorted(var arr: array of integer): Int64;
// 0<=get_9tupel_sorted < 17.643.225.600
var
  a, b, x, j, k: integer;
  permN: array [0 .. NPIECE div 2 - 1] of integer;
begin
  a := 0;
  x := 0;
  // First compute the index a < (18 choose 9) and the permutation array perm9
  // for the tiles 0..8
  for j := NPIECE - 1 downto 0 do
  begin
    if arr[j] < NPIECE div 2 then
    begin
      inc(a, c_nk(NPIECE - 1 - j, x + 1));
      permN[NPIECE div 2 - 1 - x] := arr[j];
      inc(x);
    end;
  end;

  // Then compute the index b < 9! for the permutation in perm9
  b := 0;
  for j := NPIECE div 2 - 1 downto 1 do
  begin
    k := 0;
    while permN[j] <> j do
    begin
      rot_left(permN, 0, j);
      inc(k)
    end;
    b := (j + 1) * b + k
  end;
  result := Int64(NPERM) * a + b
end;

procedure set_Ntupel_sorted(var arr: array of integer; idx: Int64);
var
  a, b, j, k, x: integer;
  permN: array [0 .. NPIECE div 2 - 1] of integer;
begin
  for j := 0 to NPIECE div 2 - 1 do
    permN[j] := j;

  b := idx mod NPERM;
  a := idx div NPERM;
  for j := 0 to NPIECE-1 do
    arr[j] := -1;
  j := 1; // generate permutation of tiles 0..8
  while j < NPIECE div 2  do
  begin
    k := b mod (j + 1);
    b := b div (j + 1);
    while k > 0 do
    begin
      rot_right(permN, 0, j);
      Dec(k);
    end;
    inc(j)
  end;
  x := NPIECE div 2; // set tiles 0..8
  for j := 0 to NPIECE - 1 do
  begin
    if a - c_nk(NPIECE - 1 - j, x) >= 0 then
    begin
      arr[j] := permN[NPIECE div 2 - x];
      Dec(a, c_nk(NPIECE - 1 - j, x));
      Dec(x);
    end;
  end;
  // Set the remainig tiles
  x := NPIECE div 2;
  for j := 0 to NPIECE - 1 do
    if arr[j] = -1 then
    begin
      arr[j] := x;
      inc(x)
    end;

end;

procedure move(var p: TPuz; cID, dir: integer);
var
  i, tmpID, tmpOri: integer;
begin
  case dir of
    0: // clockwise turn
      begin
        if cID = 0 then
        begin
          tmpID := p.id[0];
          tmpOri := p.ori[0];
          for i := 0 to NPIECE div 2 - 1 do
          begin
            p.id[i] := p.id[i + 1];
            p.ori[i] := (p.ori[i + 1] + 1) mod (NPIECE div 2 + 1);
          end;
          p.id[NPIECE div 2] := tmpID;
          p.ori[NPIECE div 2] := (tmpOri + 1) mod (NPIECE div 2 + 1);
        end
        else
        begin
          tmpID := p.id[NPIECE div 2];
          tmpOri := p.ori[NPIECE div 2];
          for i := NPIECE div 2 to NPIECE - 2 do
          begin
            p.id[i] := p.id[i + 1];
            p.ori[i] := (p.ori[i + 1] + 1) mod (NPIECE div 2 + 1);
          end;
          p.id[NPIECE - 1] := p.id[0];
          p.ori[NPIECE - 1] := (p.ori[0] + 1) mod (NPIECE div 2 + 1);
          p.id[0] := tmpID;
          p.ori[0] := (tmpOri + 1) mod (NPIECE div 2 + 1);
        end;
        p.pzo[cID] := (p.pzo[cID] + 1) mod (NPIECE div 2 + 1);
      end;
    1: // anticlockwise turn
      begin
        if cID = 0 then
        begin
          tmpID := p.id[NPIECE div 2];
          tmpOri := p.ori[NPIECE div 2];
          for i := NPIECE div 2 downto 1 do
          begin
            p.id[i] := p.id[i - 1];
            p.ori[i] := (p.ori[i - 1] + NPIECE div 2) mod (NPIECE div 2 + 1);
          end;
          p.id[0] := tmpID;
          p.ori[0] := (tmpOri + NPIECE div 2) mod (NPIECE div 2 + 1);
        end
        else
        begin
          tmpID := p.id[NPIECE - 1];
          tmpOri := p.ori[NPIECE - 1];
          for i := 17 downto (NPIECE div 2 + 1) do
          begin
            p.id[i] := p.id[i - 1];
            p.ori[i] := (p.ori[i - 1] + NPIECE div 2) mod (NPIECE div 2 + 1);
          end;
          p.id[NPIECE div 2] := p.id[0];
          p.ori[NPIECE div 2] := (p.ori[0] + NPIECE div 2)
            mod (NPIECE div 2 + 1);
          p.id[0] := tmpID;
          p.ori[0] := (tmpOri + NPIECE div 2) mod (NPIECE div 2 + 1);
        end;
        p.pzo[cID] := (p.pzo[cID] + NPIECE div 2) mod (NPIECE div 2 + 1);
      end;
  end;
end;

procedure moveN(var p: TPuz; n: integer);
var
  i, tmpID: integer;
begin
  case n of
    0: // right disk clockwise
      begin
        tmpID := p.id[0];
        for i := 0 to NPIECE div 2 - 1 do
          p.id[i] := p.id[i + 1];
        p.id[NPIECE div 2] := tmpID;
      end;
    1: // right disk anticlockwise
      begin
        tmpID := p.id[NPIECE div 2];
        for i := NPIECE div 2 downto 1 do
          p.id[i] := p.id[i - 1];
        p.id[0] := tmpID;
      end;
    2: // left disk clockwise
      begin
        tmpID := p.id[NPIECE div 2];
        for i := NPIECE div 2 to NPIECE - 2 do
          p.id[i] := p.id[i + 1];
        p.id[NPIECE - 1] := p.id[0];
        p.id[0] := tmpID;
      end;
    3: // left disk anticlockwise
      begin
        tmpID := p.id[NPIECE - 1];
        for i := NPIECE - 1 downto NPIECE div 2 + 1 do
          p.id[i] := p.id[i - 1];
        p.id[NPIECE div 2] := p.id[0];
        p.id[0] := tmpID;
      end;
  end;
end;

procedure applySequence(var man: array of integer; len: integer; var p: TPuz);
var
  i, j: integer;
begin
  for i := 0 to len - 1 do
  begin
    case man[i] of
      0 .. NPIECE div 2 - 1:
        for j := 0 to man[i] do
          move(p, 0, 0);
      NPIECE div 2 .. NPIECE - 1:
        for j := NPIECE div 2 to man[i] do
          move(p, 1, 0);
    end;
  end;
end;

procedure remap(var p, pm: TPuz);
var
  i: integer;
begin
  for i := 0 to NPIECE - 1 do
  begin
    pm.id[i] := (p.id[(i + NPIECE div 2) mod NPIECE] + NPIECE div 2) mod NPIECE;
    pm.ori[i] := p.ori[(i + NPIECE div 2) mod NPIECE];
  end;

end;

procedure setPruning(pos, val: UInt64);
/// // / 0<=val<4
var
  chunk, offset, offset64, base64: integer;
  mask: UInt64;
begin
  chunk := pos div NPERM;
  offset := pos mod NPERM;
  base64 := offset div 32; // 32 Positionen pro Int64
  offset64 := offset mod 32;
  val := val shl (offset64 * 2);
  mask := Int64(3);
  mask := mask shl (offset64 * 2);
  mask := not mask;
  pruningNCircle[chunk, base64] := pruningNCircle[chunk, base64] and mask;
  // zero bits
  pruningNCircle[chunk, base64] := pruningNCircle[chunk, base64] or val;
end;

function getPruning(pos: Int64): integer;
var
  chunk, offset, offset64, base64: integer;
  mask, val: UInt64;
begin
  chunk := pos div NPERM;
  offset := pos mod NPERM;
  base64 := offset div 32;
  // 32 Positionen pro Int64
  offset64 := offset mod 32;
  mask := Int64(3);
  mask := mask shl (offset64 * 2);
  val := pruningNCircle[chunk, base64] and mask;
  result := integer(val shr (offset64 * 2));
end;

procedure initPuzzle(var p: TPuz);
var
  i, j: integer;
begin
  for i := 0 to NPIECE - 1 do
  begin
    p.id[i] := i;
    p.ori[i] := 0;
  end;
  p.pzo[0] := 0;
  p.pzo[1] := 0;

end;

procedure loadPruning;
var
  fs: TFileStream;
var
  pz: TPuz;
  depth: UInt8;
  done, doneOld, n, i, j: Int64;
  bwsearch, hit: Boolean;
  NINT64: integer;
var
  fnk: String;
  // const
  // fn = 'pruningN';

begin
  fnk := 'pruning' + Inttostr(NPIECE div 2);
  if NPERM mod 32 > 0 then
    NINT64 := NPERM div 32 + 1
  else
    NINT64 := NPERM div 32;

  for i := 0 to NPOS - 1 do
    Setlength(pruningNCircle[i], NINT64);
  // 11340 = 9!/4/8
  if FileExists(fnk) then
  begin
    fs := TFileStream.Create(fnk, fmOpenRead);
    for i := 0 to NPOS - 1 do
      fs.ReadBuffer(pruningNCircle[i][0], NINT64 * 8);
    fs.free;
  end
  else
  begin
    for i := 0 to NPOS - 1 do
    begin
      for j := 0 to NINT64 - 1 do
        pruningNCircle[i, j] := $FFFFFFFFFFFFFFFF;
    end;

    initPuzzle(pz);
    // n := get_Ntupel_sorted(pz.id);
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    depth := 0;
    bwsearch := false;
    setPruning(PRUNID, depth);
    done := 1;
    doneOld := 0;
    while done <> doneOld do
    begin
      Form1.Memo1.Lines.Add(Inttostr(depth) + ': ' + Inttostr(done));
      Application.ProcessMessages;
      doneOld := done;
      inc(depth);

      if depth = BWSEARCHLIM[NPIECE div 2] then
        bwsearch := true;

      for i := 0 to NSTATES - 1 do
      begin
        if i mod 100000 = 0 then
          Application.ProcessMessages;
        if not bwsearch then
        begin
          if getPruning(i) = (depth - 1) mod 3 then
          // occupied
          begin
            set_Ntupel_sorted(pz.id, i); // set permutation according to i

            for j := 1 to NPIECE div 2 do
            begin
              moveN(pz, 0);
              n := get_Ntupel_sorted(pz.id);
              if getPruning(n) = 3 then // yet free
              begin
                setPruning(n, depth mod 3);
                inc(done);
              end;
            end;
            moveN(pz, 0);

            for j := 1 to NPIECE div 2 do
            begin
              moveN(pz, 2);
              n := get_Ntupel_sorted(pz.id);
              if getPruning(n) = 3 then // yet free
              begin
                setPruning(n, depth mod 3);
                inc(done);
              end;
            end;
            moveN(pz, 2);
          end;
        end
        else
        begin
          if getPruning(i) = 3 then // empty, candidate for depth
          begin
            set_Ntupel_sorted(pz.id, i);
            hit := false;
            for j := 1 to NPIECE div 2 do
            begin
              moveN(pz, 0);
              n := get_Ntupel_sorted(pz.id);
              if getPruning(n) = (depth - 1) mod 3 then
              // state already set
              begin
                setPruning(i, depth mod 3);
                inc(done);
                hit := true;
                break;
              end;
            end;
            if hit then
              Continue;
            moveN(pz, 0); // restore old state

            for j := 1 to NPIECE div 2 do
            begin
              moveN(pz, 2);
              n := get_Ntupel_sorted(pz.id);
              if getPruning(n) = (depth - 1) mod 3 then
              // state already set
              begin
                setPruning(i, depth mod 3);
                inc(done);
                break;
              end;
            end;
          end;
        end;
      end;
    end;
    Form1.Memo1.Lines.Add(Inttostr(depth) + ': ' + Inttostr(done));
    fs := TFileStream.Create(fnk, fmCreate);
    for i := 0 to NPOS - 1 do
      fs.WriteBuffer(pruningNCircle[i][0], NINT64 * 8);
    fs.free;
  end;
end;

function getTrueDist(pz: TPuz): integer;
var
  i, n, nnew: integer;
  idx, idx2: Int64;
  hit: Boolean;
begin
  result := 0;
  idx := get_Ntupel_sorted(pz.id);
  while idx <> PRUNID do
  begin
    hit := false;
    for i := 0 to NPIECE div 2 - 1 do
    begin
      moveN(pz, 0);
      idx2 := get_Ntupel_sorted(pz.id);
      if (getPruning(idx) - getPruning(idx2) + 3) mod 3 = 1 then
      begin
        idx := idx2;
        inc(result);
        hit := true;
        break;
      end;
    end;
    if hit then
      Continue;
    moveN(pz, 0); // restore state

    for i := 0 to NPIECE div 2 - 1 do
    begin
      moveN(pz, 2);
      idx2 := get_Ntupel_sorted(pz.id);
      if (getPruning(idx) - getPruning(idx2) + 3) mod 3 = 1 then
      begin
        idx := idx2;
        inc(result);
        hit := true;
        break;
      end;
    end;
    if hit then
      Continue;
    Form1.Memo1.Lines.Add('Error while computing true depth.');
  end;
end;

function toString(var a: array of integer; len: integer): String;
var
  i: integer;
begin
  result := '';
  for i := 0 to len - 1 do
  begin
    case a[i] of
      0 .. NPIECE div 2 - 1:
        result := result + 'R' + Inttostr(a[i] + 1) + ' ';
      NPIECE div 2 .. NPIECE - 1:
        result := result + 'L' + Inttostr(a[i] + 1 - (NPIECE div 2)) + ' ';
    end;
  end;
end;

function getNewPrun(var pz: TPuz; oldPrun: integer): integer;
// compute new pruning value from old pruning value and the new table mod 3 entry
var
  n: integer;
begin
  n := getPruning(get_Ntupel_sorted(pz.id));
  case ((n + 3 - oldPrun mod 3)) mod 3 of
    0:
      result := oldPrun;
    1:
      result := oldPrun + 1;
    2:
      result := oldPrun - 1;
  end;
end;

function twist(disk: integer; var maneuver: array of integer;
  length: integer): integer;
var
  i: integer;
begin
  result := 0;
  if disk = 0 then
  begin
    for i := 0 to length - 1 do
    begin
      if maneuver[i] < NPIECE div 2 then
        inc(result, maneuver[i] + 1)
    end
  end
  else
  begin
    for i := 0 to length - 1 do
    begin
      if maneuver[i] >= NPIECE div 2 then // disk 1 (left) rotation
        inc(result, maneuver[i] - (NPIECE div 2 - 1))
    end
  end;
  result := (10 * (NPIECE div 2 + 1) + result) mod (NPIECE div 2 + 1);
end;

procedure search(var pz: TPuz; len, togo, prun0, prun1: integer);
var
  tw0, tw1, i: Int64;
  pr, po: TPuz;
begin
  if solved then
    exit;

  Application.ProcessMessages;
  if max(prun0, prun1) > togo then
    exit;
  if (togo = 0) then
  begin
    tw0 := twist(0, sofar, len);
    tw1 := twist(1, sofar, len);

    initPuzzle(po);
    applySequence(sofar, len, po);



    for i  := 0 to NPIECE-1 do  // Fix orientation of pieces
      if po.ori[i] <> 0 then
        exit;


    // if tw0 <> 0 then
    // exit;
    // if tw1 <> 0 then
    // exit;



    Form1.Memo1.Lines.Add(toString(sofar, len) + ' (' + Inttostr(len) + '), ' +
      'twist: (' + Inttostr(tw0) + ', ' + Inttostr(tw1) + ')');
//    Form1.Memo1.Lines.Add(Inttostr(po.ori[0]) + ' ' +
//      Inttostr(po.ori[NPIECE div 2]));
    Application.ProcessMessages;
    // solved := true;
  end
  else
  begin
    if len - togo > 0 then // already at least one move done
    begin
      if sofar[len - togo - 1] < NPIECE div 2 then // disk 0 turn before
      begin
        for i := 0 to NPIECE div 2 - 1 do // now disk 1 turns
        begin
          moveN(pz, 2);
          sofar[len - togo] := NPIECE div 2 + i; // turn of left disk by i
          remap(pz, pr); // rotation by 180° and reindexing
          search(pz, len, togo - 1, getNewPrun(pz, prun0),
            getNewPrun(pr, prun1));
        end;
        moveN(pz, 2); // restore puzzle state
      end
      else // disk 1 turn before
      begin
        for i := 0 to NPIECE div 2 - 1 do // now disk 0 turns
        begin
          moveN(pz, 0);
          sofar[len - togo] := i; // turn of right disk by i
          remap(pz, pr); // rotation by 180° and reindexing
          search(pz, len, togo - 1, getNewPrun(pz, prun0),
            getNewPrun(pr, prun1));
        end;
        moveN(pz, 0); // restore puzzle state
      end;
    end
    else // first move, len=togo
    begin
      for i := 0 to NPIECE div 2 - 1 do // first turn on disk 0
      begin
        moveN(pz, 0);
        sofar[len - togo] := i;
        remap(pz, pr);
        search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
      end;
      moveN(pz, 0); // restore puzzle state

      for i := 0 to NPIECE div 2 - 1 do // first turn on disk 1
      begin
        moveN(pz, 2);
        sofar[len - togo] := i + NPIECE div 2;
        remap(pz, pr);
        search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
      end;
      moveN(pz, 2); // restore puzzle stated
    end;
  end;
end;

procedure findsolution;
var
  p, pm: TPuz;
  a, b, c, d, ln, prun0, prun1: integer;
begin
  initPuzzle(p);
  // jetzt ggf. Änderungen vornehmen
  // moveN(p, 0);
  // moveN(p, 0);
  // moveN(p, 2);
  // moveN(p, 2);
  // moveN(p, 3);
  // moveN(p, 1);

//   a := 0;
//   b := NPIECE div 2;
//   c := b+1;
//   d := b-1;
//   p.id[a] := b;
//   p.id[b] := a;
//   p.id[c] := d;
//   p.id[d] := c;

//   a := 0;
//   b := 8;
//   c := 10;
//   p.id[b] := a;
//   p.id[c] := b;
//   p.id[a] := c;

//   a:=0;
//   b:=NPIECE div 2;
//   p.id[a] := b;
//   p.id[b] := a;

   a:=NPIECE div 2-1;
   b:=NPIECE div 2+1;
   p.id[a] := b;
   p.id[b] := a;




  solved := false;
  ln := 0;
  prun0 := getTrueDist(p);
  remap(p, pm);
  prun1 := getTrueDist(pm);
  Form1.Memo1.Lines.Add(Inttostr(prun0) + '   ' + Inttostr(prun1));

  while not solved do
  begin
    Form1.Memo1.Lines.Add(Inttostr(ln));
    search(p, ln, ln, prun0, prun1);
    inc(ln);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pz: TPuz;
  i, n: Int64;
begin

//  for i := 0 to NSTATES - 1 do
//  begin
//    set_Ntupel_sorted(pz.id, i);
//    n := get_Ntupel_sorted(pz.id);
//    if n <> i then
//      n := 0;
//  end;

  loadPruning;
  findsolution;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  solved := true;
  Memo1.Lines.Add('stopped!')
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  pz: TPuz;
begin
  // pruningNCircle: array [0 .. 48620 - 1] of array of UInt64; // 48620 = 18 choose 9
  NPOS := c_nk(NPIECE, NPIECE div 2);
  NPERM := nfac(NPIECE div 2);
  NSTATES := Int64(NPOS) * NPERM;
  Setlength(pruningNCircle, NPOS);
  // prunID = 17642862720;
  initPuzzle(pz);
  PRUNID := get_Ntupel_sorted(pz.id);
end;

end.
