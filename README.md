# ProjetoPedidosLanchonete
aplicação para gerenciar uma lanchonete, com funcionalidades de cardápio, gerenciamento de lanches e ingredientes, gerenciamento de pedidos e autenticação de usuários. A aplicação deve ser desenvolvida para desktop.
<img width="919" height="644" alt="image" src="https://github.com/user-attachments/assets/45b0aa30-d651-414c-86d3-3ec29403a4e6" />

<img width="1022" height="696" alt="image" src="https://github.com/user-attachments/assets/43149d2d-3fe6-4c09-985d-1229885a00ad" />

<img width="1896" height="988" alt="image" src="https://github.com/user-attachments/assets/f7971488-251b-4736-a722-0b576f13963d" />

<img width="1119" height="749" alt="image" src="https://github.com/user-attachments/assets/6667f383-14ea-436b-b05f-415ce5cc5416" />

# Instalação
O Processo de instalação é simples, o próprio executável da aplicação, na primeira execução, procura se já existe o banco de dados criado e configurado para a aplicação, se não tiver, a própria aplicação cria o banco com base no dicionário de dados embutido no código, e cria o arquivo de configuração ( Pedidos.ini ) onde é possível apontar para o banco de dados, após esse arquivo de configuração criado, é possível mudar o banco de dados para qualquer diretório, e ajustar o caminho do mesmo na propriedade "Database"

Usuário com perfil de administrador: 
email: ADMIN@admin.com.br
senha: 123456

Usuário com perfil de operador:
email: USUARIO@usuario.com.br
senha: 654321

# Sobre o Projeto
Aplicação foi desenvolvida utilizando o Delphi Community Edition e banco SQLite, utilizandos componentes nativos da ferramenta, sem dependencia de componentes de terceiros, e com grande rastreabilidade via logs, algo que considero essencial para manutenção de aplicativos.

Procurei focar mais na parte de estrutura e programação do mesmo, do que na parte visual, portanto daria para melhorar bastante essa parte visual ainda, mas entendo que se trata de um teste de conhecimento, e não o desenvolvimento de uma ferramenta para ser comercializada. 
