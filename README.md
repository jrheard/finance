# finance

A Clojure program that takes a .csv dump of financial transactions from your Mint account and can then tell you things about how you've been spending your money.

Well, I say "you", but really I just mean "me". I just wrote this because none of the finance webapps I use was able to answer the question "How much money did I make in 2012? Not counting investments, and excluding duplicate transactions, how much money did I spend in 2012?".

I also wrote it because I wanted to write a program so I could try out [vim-foreplay](https://github.com/tpope/vim-foreplay) and this was the first thing I thought of. This definitely won't accurately report things about your .csv without some code tweaks of your own.

## Usage

    mkdir resources
    mv a_mint_transaction_dump.csv resources/
    lein run

## License

All right reserved, until I get around to reading about licenses and figuring out which one I like.
