アドホック多相をMLインタプリタに実装する

# アドホック多相
アドホック多相とは関数の定義を型ごとに変更する機能です。
例えばC++のオーバーロードがアドホック多相の例として挙げられます。

# MLインタプリタへの実装
このアドホック多相を実現する最も簡単な方法は、多重定義されている関数を全通り試すというものです。

# Z3を使って解いてみる