object FormUsuario: TFormUsuario
  Left = 0
  Top = 0
  Caption = 'Cadastro de Usu'#225'rios'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = clNavy
    ParentBackground = False
    TabOrder = 0
    object lblTitulo: TLabel
      Left = 16
      Top = 16
      Width = 328
      Height = 29
      Caption = 'Gerenciamento de Usu'#225'rios'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 560
    Width = 900
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 60
    Width = 900
    Height = 500
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 900
      Height = 500
      ActivePage = tsCadastro
      Align = alClient
      TabOrder = 0
      object tsLista: TTabSheet
        Caption = 'Lista de Usu'#225'rios'
        object pnlFiltro: TPanel
          Left = 0
          Top = 0
          Width = 892
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFiltro: TLabel
            Left = 16
            Top = 16
            Width = 50
            Height = 13
            Caption = 'Pesquisar:'
          end
          object edtFiltro: TEdit
            Left = 88
            Top = 13
            Width = 300
            Height = 21
            TabOrder = 0
            OnChange = edtFiltroChange
          end
          object chkApenasAtivos: TCheckBox
            Left = 408
            Top = 15
            Width = 120
            Height = 17
            Caption = 'Apenas Ativos'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = chkApenasAtivosClick
          end
        end
        object GridUsuarios: TStringGrid
          Left = 0
          Top = 50
          Width = 892
          Height = 372
          Align = alClient
          ColCount = 6
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
          TabOrder = 1
          OnDblClick = GridUsuariosDblClick
        end
        object pnlBotoesLista: TPanel
          Left = 0
          Top = 422
          Width = 892
          Height = 50
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object btnNovo: TButton
            Left = 16
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Novo'
            TabOrder = 0
            OnClick = btnNovoClick
          end
          object btnEditar: TButton
            Left = 122
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Editar'
            TabOrder = 1
            OnClick = btnEditarClick
          end
          object btnExcluir: TButton
            Left = 228
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Excluir'
            TabOrder = 2
            OnClick = btnExcluirClick
          end
          object btnAtualizar: TButton
            Left = 334
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Atualizar'
            TabOrder = 3
            OnClick = btnAtualizarClick
          end
        end
      end
      object tsCadastro: TTabSheet
        Caption = 'Cadastro'
        ImageIndex = 1
        object lblNome: TLabel
          Left = 24
          Top = 24
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object lblEmail: TLabel
          Left = 24
          Top = 75
          Width = 32
          Height = 13
          Caption = 'E-mail:'
        end
        object lblSenha: TLabel
          Left = 24
          Top = 126
          Width = 34
          Height = 13
          Caption = 'Senha:'
        end
        object lblConfirmaSenha: TLabel
          Left = 24
          Top = 177
          Width = 84
          Height = 13
          Caption = 'Confirmar Senha:'
        end
        object lblTipo: TLabel
          Left = 24
          Top = 228
          Width = 24
          Height = 13
          Caption = 'Tipo:'
        end
        object edtNome: TEdit
          Left = 24
          Top = 43
          Width = 400
          Height = 21
          TabOrder = 0
        end
        object edtEmail: TEdit
          Left = 24
          Top = 94
          Width = 400
          Height = 21
          TabOrder = 1
        end
        object edtSenha: TEdit
          Left = 24
          Top = 145
          Width = 250
          Height = 21
          PasswordChar = '*'
          TabOrder = 2
        end
        object edtConfirmaSenha: TEdit
          Left = 24
          Top = 196
          Width = 250
          Height = 21
          PasswordChar = '*'
          TabOrder = 3
        end
        object cmbTipo: TComboBox
          Left = 24
          Top = 247
          Width = 200
          Height = 21
          Style = csDropDownList
          TabOrder = 4
        end
        object chkAtivo: TCheckBox
          Left = 24
          Top = 286
          Width = 97
          Height = 17
          Caption = 'Ativo'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object pnlBotoesCadastro: TPanel
          Left = 0
          Top = 422
          Width = 892
          Height = 50
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 6
          object btnSalvar: TButton
            Left = 24
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Salvar'
            TabOrder = 0
            OnClick = btnSalvarClick
          end
          object btnCancelar: TButton
            Left = 130
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Cancelar'
            TabOrder = 1
            OnClick = btnCancelarClick
          end
        end
      end
    end
  end
end
