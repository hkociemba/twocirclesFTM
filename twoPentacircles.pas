unit twoPentacircles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TTileID = array [0 .. 17] of Integer;
  TTileOri = array [0 .. 17] of Integer;
  TPuzOri = array [0 .. 1] of Integer;

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

const
  prunID = 17642862720;

var
  pruning9: array [0 .. 48620 - 1] of array of UInt64; // 48620 = 18 choose 9
  sofar: array [0 .. 50] of Integer;
  solved: Boolean;

function c_nk(n, k: Integer): Integer;
var
  i, j: Integer;
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

procedure rot_right(var a: Array of Integer; l, r: Integer);
// Rotate array arr right between l and r. r is included.
var
  tmp, i: Integer;
begin
  tmp := a[r];
  for i := r downto l + 1 do
    a[i] := a[i - 1];
  a[l] := tmp
end;

procedure rot_left(var a: Array of Integer; l, r: Integer);
// Rotate array arr left between l and r. r is included.
var
  tmp, i: Integer;
begin
  tmp := a[l];
  for i := l to r - 1 do
    a[i] := a[i + 1];
  a[r] := tmp
end;

function get_9tupel_sorted(var arr: array of Integer): Int64;
// 0<=get_9tupel_sorted < 17.643.225.600
var
  a, b, x, j, k: Integer;
  perm9: array [0 .. 8] of Integer;
begin
  a := 0;
  x := 0;
  // First compute the index a < (18 choose 9) and the permutation array perm9
  // for the tiles 0..8
  for j := 17 downto 0 do
  begin
    if arr[j] < 9 then
    begin
      inc(a, c_nk(17 - j, x + 1));
      perm9[8 - x] := arr[j];
      inc(x);
    end;
  end;

  // Then compute the index b < 9! for the permutation in perm9
  b := 0;
  for j := 8 downto 1 do
  begin
    k := 0;
    while perm9[j] <> j do
    begin
      rot_left(perm9, 0, j);
      inc(k)
    end;
    b := (j + 1) * b + k
  end;
  result := Int64(362880) * a + b
end;

procedure set_9tupel_sorted(var arr: array of Integer; idx: Int64);
var
  a, b, j, k, x: Integer;
  perm9: array [0 .. 8] of Integer;
begin
  for j := 0 to 8 do
    perm9[j] := j;

  b := idx mod 362880;
  a := idx div 362880;
  for j := 0 to 17 do
    arr[j] := -1;
  j := 1; // generate permutation of tiles 0..8
  while j < 9 do
  begin
    k := b mod (j + 1);
    b := b div (j + 1);
    while k > 0 do
    begin
      rot_right(perm9, 0, j);
      Dec(k);
    end;
    inc(j)
  end;
  x := 9; // set tiles 0..8
  for j := 0 to 17 do
  begin
    if a - c_nk(17 - j, x) >= 0 then
    begin
      arr[j] := perm9[9 - x];
      Dec(a, c_nk(17 - j, x));
      Dec(x);
    end;
  end;
  // Set the remainig tiles
  x := 9;
  for j := 0 to 17 do
    if arr[j] = -1 then
    begin
      arr[j] := x;
      inc(x)
    end;

end;

procedure initPuzzle(var p: TPuz);
var
  i: Integer;
begin
  for i := 0 to 17 do
  begin
    p.id[i] := i;
    p.ori[i] := 0;
  end;
  p.pzo[0] := 0;
  p.pzo[1] := 0;
end;

procedure move(var p: TPuz; cID, dir: Integer);
var
  i, tmpID, tmpOri: Integer;
begin
  case dir of
    0: // clockwise turn
      begin
        if cID = 0 then
        begin
          tmpID := p.id[0];
          tmpOri := p.ori[0];
          for i := 0 to 8 do
          begin
            p.id[i] := p.id[i + 1];
            p.ori[i] := (p.ori[i + 1] + 1) mod 10;
          end;
          p.id[9] := tmpID;
          p.ori[9] := (tmpOri + 1) mod 10;
        end
        else
        begin
          tmpID := p.id[9];
          tmpOri := p.ori[9];
          for i := 9 to 16 do
          begin
            p.id[i] := p.id[i + 1];
            p.ori[i] := (p.ori[i + 1] + 1) mod 10;;
          end;
          p.id[17] := p.id[0];
          p.ori[17] := (p.ori[0] + 1) mod 10;
          p.id[0] := tmpID;
          p.ori[0] := (tmpOri + 1) mod 10;
        end;
        p.pzo[cID] := (p.pzo[cID] + 1) mod 10;
      end;
    1: // anticlockwise turn
      begin
        if cID = 0 then
        begin
          tmpID := p.id[9];
          tmpOri := p.ori[9];
          for i := 9 downto 1 do
          begin
            p.id[i] := p.id[i - 1];
            p.ori[i] := (p.ori[i - 1] + 9) mod 10;
          end;
          p.id[0] := tmpID;
          p.ori[0] := (tmpOri + 9) mod 10;
        end
        else
        begin
          tmpID := p.id[17];
          tmpOri := p.ori[17];
          for i := 17 downto 10 do
          begin
            p.id[i] := p.id[i - 1];
            p.ori[i] := (p.ori[i - 1] + 9) mod 10;
          end;
          p.id[9] := p.id[0];
          p.ori[9] := (p.ori[0] + 9) mod 10;
          p.id[0] := tmpID;
          p.ori[0] := (tmpOri + 9) mod 10;
        end;
        p.pzo[cID] := (p.pzo[cID] + 9) mod 10;
      end;
  end;
end;

procedure moveN(var p: TPuz; n: Integer);
var
  i, tmpID: Integer;
begin
  case n of
    0:
      begin
        tmpID := p.id[0];
        for i := 0 to 8 do
          p.id[i] := p.id[i + 1];
        p.id[9] := tmpID;
      end;
    1:
      begin
        tmpID := p.id[9];
        for i := 9 downto 1 do
          p.id[i] := p.id[i - 1];
        p.id[0] := tmpID;
      end;
    2:
      begin
        tmpID := p.id[9];
        for i := 9 to 16 do
          p.id[i] := p.id[i + 1];
        p.id[17] := p.id[0];
        p.id[0] := tmpID;
      end;
    3:
      begin
        tmpID := p.id[17];
        for i := 17 downto 10 do
          p.id[i] := p.id[i - 1];
        p.id[9] := p.id[0];
        p.id[0] := tmpID;
      end;
  end;
end;

procedure remap(var p, pm: TPuz);
var
  i: Integer;
begin
  for i := 0 to 17 do
  begin
    pm.id[i] := (p.id[(i + 9) mod 18] + 9) mod 18;
    pm.ori[i] := p.ori[(i + 9) mod 18];
  end;
  // for i := 0 to 17 do
  // begin
  // p.id[i] := pm.id[i];
  // p.ori[i] := pm.ori[i];
  // end;
end;

procedure setPruning(pos, val: Int64);
/// // / 0<=val<4
var
  chunk, offset, offset64, base64: Integer;
  mask: Int64;
begin
  chunk := pos div 362880;
  offset := pos mod 362880;
  base64 := offset div 32; // 32 Positionen pro Int64
  offset64 := offset mod 32;
  val := val shl (offset64 * 2);
  mask := Int64(3);
  mask := mask shl (offset64 * 2);
  mask := not mask;
  pruning9[chunk, base64] := pruning9[chunk, base64] and mask; // zero bits
  pruning9[chunk, base64] := pruning9[chunk, base64] or val;
end;

function getPruning(pos: Int64): Integer;
var
  chunk, offset, offset64, base64: Integer;
  mask, val: Int64;
begin
  chunk := pos div 362880;
  offset := pos mod 362880;
  base64 := offset div 32; // 32 Positionen pro Int64
  offset64 := offset mod 32;
  mask := Int64(3);
  mask := mask shl (offset64 * 2);
  val := pruning9[chunk, base64] and mask;
  result := Integer(val shr (offset64 * 2));
end;

procedure loadPruning;
var
  fs: TFileStream;
var
  pz: TPuz;
  depth: UInt8;
  done, doneOld, n, i, j: Int64;
  bwsearch: Boolean;
const
  fn = 'pruning9';

begin
  for i := 0 to 48620 - 1 do
    Setlength(pruning9[i], 11340); // 11340 = 9!/4/8
  if FileExists(fn) then
  begin
    fs := TFileStream.Create(fn, fmOpenRead);
    for i := 0 to 48620 - 1 do
      fs.ReadBuffer(pruning9[i][0], 11340 * 8);
    fs.free;
  end
  else
  begin
    for i := 0 to 48620 - 1 do
    begin
      for j := 0 to 11340 - 1 do
        pruning9[i, j] := $FFFFFFFFFFFFFFFF;
    end;

    initPuzzle(pz);
    n := get_9tupel_sorted(pz.id);
    depth := 0;
    bwsearch := false;
    setPruning(n, depth);
    done := 1;
    doneOld := 0;
    while done <> doneOld do
    begin
      Form1.Memo1.Lines.Add(Inttostr(depth) + ': ' + Inttostr(done));
      Application.ProcessMessages;
      doneOld := done;
      inc(depth);

      if depth = 27 then
        bwsearch := true;

      for i := 0 to 17643225600 - 1 do
      begin
        if i mod 100000 = 0 then
          Application.ProcessMessages;
        if not bwsearch then
        begin
          if getPruning(i) = (depth - 1) mod 3 then // occupied
          begin
            set_9tupel_sorted(pz.id, i);
            move(pz, 0, 0); // rotate disk 0 clockwise
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = 3 then // yet free
            begin
              setPruning(n, depth mod 3);
              inc(done);
            end;
            move(pz, 0, 1); // rotate disk 0 anticlockwise
            move(pz, 0, 1);
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = 3 then // yet free
            begin
              setPruning(n, depth mod 3);
              inc(done);
            end;
            move(pz, 0, 0);

            move(pz, 1, 0); // rotate disk 1 clockwise
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = 3 then // yet free
            begin
              setPruning(n, depth mod 3);
              inc(done);
            end;
            move(pz, 1, 1); // rotate disk 1 anticlockwise
            move(pz, 1, 1);
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = 3 then // yet free
            begin
              setPruning(n, depth mod 3);
              inc(done);
            end;
          end;
        end
        else
        begin
          if getPruning(i) = 3 then // empty, candidate for depth
          begin
            set_9tupel_sorted(pz.id, i);
            move(pz, 0, 0); // rotate disk 0 clockwise
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = (depth - 1) mod 3 then // state already set
            begin
              setPruning(i, depth mod 3);
              inc(done);
              continue;
            end;
            move(pz, 0, 1); // rotate disk 0 anticlockwise
            move(pz, 0, 1);
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = (depth - 1) mod 3 then // state already set
            begin
              setPruning(i, depth mod 3);
              inc(done);
              continue;
            end;
            move(pz, 0, 0);

            move(pz, 1, 0); // rotate disk 1 clockwise
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = (depth - 1) mod 3 then // state already set
            begin
              setPruning(i, depth mod 3);
              inc(done);
              continue;
            end;
            move(pz, 1, 1); // rotate disk 1 anticlockwise
            move(pz, 1, 1);
            n := get_9tupel_sorted(pz.id);
            if getPruning(n) = (depth - 1) mod 3 then // state already set
            begin
              setPruning(i, depth mod 3);
              inc(done);
              continue;
            end;
          end;
        end;

      end;
    end;
    Form1.Memo1.Lines.Add(Inttostr(depth) + ': ' + Inttostr(done));
    // fs := TFileStream.Create('F:\!Data\Programmieren\XE8\pentarot\Win64\Debug\test', fmCreate);
    fs := TFileStream.Create(fn, fmCreate);
    for i := 0 to 48620 - 1 do
      fs.WriteBuffer(pruning9[i][0], 11340 * 8);
    fs.free;
  end;
end;

function getTrueDist(pz: TPuz): Integer;
var
  n, nnew: Integer;
  idx, idx2: Int64;
begin
  result := 0;
  idx := get_9tupel_sorted(pz.id);
  while idx <> prunID do
  begin
    move(pz, 0, 0);
    idx2 := get_9tupel_sorted(pz.id);
    if (getPruning(idx) - getPruning(idx2) + 3) mod 3 = 1 then
    begin
      idx := idx2;
      inc(result);
      continue;
    end;
    move(pz, 0, 1);
    move(pz, 0, 1);
    idx2 := get_9tupel_sorted(pz.id);
    if (getPruning(idx) - getPruning(idx2) + 3) mod 3 = 1 then
    begin
      idx := idx2;
      inc(result);
      continue;
    end;
    move(pz, 0, 0);
    move(pz, 1, 0);
    idx2 := get_9tupel_sorted(pz.id);
    if (getPruning(idx) - getPruning(idx2) + 3) mod 3 = 1 then
    begin
      idx := idx2;
      inc(result);
      continue;
    end;
    move(pz, 1, 1);
    move(pz, 1, 1);
    idx2 := get_9tupel_sorted(pz.id);
    if (getPruning(idx) - getPruning(idx2) + 3) mod 3 = 1 then
    begin
      idx := idx2;
      inc(result);
      continue;
    end;
    Form1.Memo1.Lines.Add('Error while computing true depth.');
  end;
end;

function toString(var a: array of Integer; len: Integer): String;
var
  i: Integer;
begin
  result := '';
  for i := 0 to len - 1 do
  begin
    case a[i] of
      0:
        result := result + 'R ';
      1:
        result := result + 'R'' ';
      2:
        result := result + 'L ';
      3:
        result := result + 'L'' ';
    end;
  end;
end;

function getNewPrun(var pz: TPuz; oldPrun: Integer): Integer;
// compute new pruning value from old pruning value and the new table mod 3 entry
var
  n: Integer;
begin
  n := getPruning(get_9tupel_sorted(pz.id));
  case ((n + 3 - oldPrun mod 3)) mod 3 of
    0:
      result := oldPrun;
    1:
      result := oldPrun + 1;
    2:
      result := oldPrun - 1;
  end;
end;

function twist(disk: Integer; var maneuver: array of Integer;
  length: Integer): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to length - 1 do
  begin
    if maneuver[i] = 2 * disk then
      inc(result);
    if maneuver[i] = 2 * disk + 1 then
      Dec(result);
  end;
  result := (100 + result) mod 10;
end;

procedure search(var pz: TPuz; len, togo, prun0, prun1: Integer);
var
  tw0, tw1, i: Int64;
  pr: TPuz;
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

    Form1.Memo1.Lines.Add(toString(sofar, len) + ' (' + Inttostr(len) + '), ' +
      'twist: (' + Inttostr(tw0) + ', ' + Inttostr(tw1) + ')');
    Application.ProcessMessages;
    // solved := true;
  end
  else
  begin
    if len - togo > 0 then
    begin
      if sofar[len - togo - 1] <> 1 then
      begin
        moveN(pz, 0);
        sofar[len - togo] := 0;

        remap(pz, pr);
        search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
        moveN(pz, 1); // undo move
      end;
      if sofar[len - togo - 1] <> 0 then
      begin
        moveN(pz, 1);
        sofar[len - togo] := 1;

        remap(pz, pr);
        search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
        moveN(pz, 0); // undo move
      end;
      if sofar[len - togo - 1] <> 3 then
      begin
        moveN(pz, 2);
        sofar[len - togo] := 2;

        remap(pz, pr);
        search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
        moveN(pz, 3); // undo move
      end;
      if sofar[len - togo - 1] <> 2 then
      begin
        moveN(pz, 3);
        sofar[len - togo] := 3;

        remap(pz, pr);
        search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
        moveN(pz, 2); // undo move
      end;
    end
    else
    begin
      moveN(pz, 0);
      sofar[len - togo] := 0;

      remap(pz, pr);
      search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
      moveN(pz, 1); // undo move

      moveN(pz, 1);
      sofar[len - togo] := 1;

      remap(pz, pr);
      search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
      moveN(pz, 0); // undo move

      moveN(pz, 2);
      sofar[len - togo] := 2;

      remap(pz, pr);
      search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
      moveN(pz, 3); // undo move

      moveN(pz, 3);
      sofar[len - togo] := 3;

      remap(pz, pr);
      search(pz, len, togo - 1, getNewPrun(pz, prun0), getNewPrun(pr, prun1));
      moveN(pz, 2); // undo move
    end;
  end;
end;

procedure findsolution;
var
  p, pm: TPuz;
  a, b, c, d, ln, prun0, prun1: Integer;
begin
  initPuzzle(p); // jetzt ggf. Änderungen vornehmen
  // moveN(p, 0);
  // moveN(p, 0);
  // moveN(p, 0);
  // moveN(p, 0);
  // moveN(p, 3);
  // moveN(p, 1);

  // a := 1;
  // b := 2;
  // c := 10;
  // d := 11;
  // p.id[a] := b;
  // p.id[b] := a;
  // p.id[c] := d;
  // p.id[d] := c;

  a := 0;
  b := 14;
  c := 15;
  p.id[a] := b;
  p.id[b] := c;
  p.id[c] := a;

  solved := false;
  /// ///////////////!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ln := 0;
  /// ///////////////!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  prun0 := getTrueDist(p);
  remap(p, pm);
  prun1 := getTrueDist(pm);
  Form1.Memo1.Lines.Add(Inttostr(prun0) + '   ' + Inttostr(prun1));

  while not solved do
  begin
    Form1.Memo1.Lines.Add(Inttostr(ln));
    search(p, ln, ln, prun0, prun1);
    inc(ln, 2);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pz: TPuz;
  n: Int64;
begin
  // initPuzzle(pz);
  // n := get_9tupel_sorted(pz.id);

  loadPruning;
  findsolution;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  solved := true;
  Memo1.Lines.Add('stopped!')
end;

end.
