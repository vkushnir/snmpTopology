unit Utils;

interface

type
  TMAC = String[6];
  TSMAC = String[19];


  function Bin2MAC(BMAC: TMAC): TSMAC;

implementation

function Bin2MAC(BMAC: TMAC): TSMAC;
var
  i: Integer;
  SMAC: TSMAC;
begin
  SMAC := '00:00:00:00:00:00';
  asm
    lea ecx, BMAC
    lea edx, SMAC
    add ecx, 1
    add edx, 1
    mov bl, ':'
    mov bh, 6

  @Do:
    mov ah, [ecx]
    and ah, $0F
    mov al, [ecx]
    and al, $F0
    shr al, 4

    add ah, '0'
    cmp ah, '9'
    jbe @Next1
    add ah, 7

  @Next1:
    add al, '0'
    cmp al, '9'
    jbe @Next2
    add al, 7

  @Next2:
    mov [edx], ax
    mov [edx + 2], bl

    add ecx, 1
    add edx, 3

    sub bh, 1
    jnz @Do
  end;
  Result := SMAC;
end;
end.
