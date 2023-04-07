output_file "output.txt";

main(argc, argv) {
    auto start_number, end_number, number_prefix, number_suffix, terminal_out, prefix_len, suffix_len, current_number;
    extrn fout, output_file;

    if (argc < 5) {
        printf("%s", "Usage:*nnumgen {start number} {end number} {prefix} {suffix}*n");
        exit(1);
    }

    fout = creat(output_file, 0664);

    start_number = string_to_int(argv[1]);

    end_number = string_to_int(argv[2]);

    number_prefix = argv[3];
    prefix_len = len(number_prefix);

    number_suffix = argv[4];
    suffix_len = len(number_suffix);

    current_number = start_number;
    while (current_number <= end_number) {
        write(fout, number_prefix, prefix_len - 1);
        printn(current_number, 10);
        write(fout, number_suffix, suffix_len - 1);
        putchar(10);
        current_number++;
    }
}

len(string) {
    auto i;
    i = 0;
    while (char(string, i++) != '*e') {}
    return (i);
}

string_to_int(string) {
    auto integer, i, strlen;
    integer = 0;
    i = 0;
    strlen = len(string);
    while (i < strlen - 1) {
        integer =* 10;
        integer =+ char(string, i) - 48;
        i++;
    }

    return (integer);
}