unit uCriptografia;

interface

uses
  System.SysUtils, System.Hash;

type
  TCriptografia = class
  public
    class function CriptografarSenha(const Senha: string): string;
    class function VerificarSenha(const Senha, SenhaHash: string): Boolean;
  end;

implementation

{ TCriptografia }

class function TCriptografia.CriptografarSenha(const Senha: string): string;
begin
  // Utilizando SHA256 para criptografar a senha
  Result := THashSHA2.GetHashString(Senha, THashSHA2.TSHA2Version.SHA256);
end;

class function TCriptografia.VerificarSenha(const Senha, SenhaHash: string): Boolean;
begin
  Result := CriptografarSenha(Senha) = SenhaHash;
end;

end.
