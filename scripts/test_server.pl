my $ekho_client = './ekho --debug --request "我挥一挥衣袖，不带走一片云彩"';

my $people = 0;

BIRTH:
if (fork()) {
    # parent
    if (++$people < 100) {
        goto BIRTH;
    }
} else {
    # child
    for (my $times; $times < 60; ++$times) {
        system("$ekho_client -o output.$times.$people.mp3");
    }
}

while (wait() != -1) {}
