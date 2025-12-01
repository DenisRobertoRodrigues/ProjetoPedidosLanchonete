object DMDTO: TDMDTO
  OnDestroy = DataModuleDestroy
  Height = 480
  Width = 640
  object FDConexao: TFDConnection
    Params.Strings = (
      'DriverID=SQLite'
      
        'Database=C:\Projetos\ProjetoPedidosLanchonete\Win32\Debug\DBPedi' +
        'dos.db')
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    UpdateOptions.AutoCommitUpdates = True
    LoginPrompt = False
    Left = 40
    Top = 24
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 192
    Top = 24
  end
  object FDQuery1: TFDQuery
    Connection = FDConexao
    SQL.Strings = (
      'SELECT * FROM TB_USUARIOS')
    Left = 112
    Top = 168
  end
end
