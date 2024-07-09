MODULE Numgen;
FROM TextIO IMPORT WriteString, WriteChar, WriteLn, ReadString, SkipLine;
FROM IOChan IMPORT Flush;
FROM StreamFile IMPORT ChanId, Open, write, Close, OpenResults;
FROM StdChans IMPORT StdInChan, StdOutChan;
IMPORT Strings;
IMPORT DynamicStrings;
IMPORT StringConvert;
IMPORT WholeConv;
IMPORT WholeIO;

TYPE STRING = ARRAY[0..1000] OF CHAR;

PROCEDURE ReadLine(stdout_string: STRING) : STRING;
VAR
    stdin_string: STRING;
BEGIN
    stdin_chan := StdInChan();
    stdout_chan := StdOutChan();
    WriteString(stdout_chan, stdout_string);
    WriteLn(stdout_chan);
    ReadString(stdin_chan, stdin_string);
    SkipLine(stdin_chan);
    RETURN stdin_string;
END ReadLine;

VAR
    start_number_string: STRING;
    start_number_dynstring: DynamicStrings.String;
    start_number: CARDINAL;
    end_number_string: STRING;
    end_number_dynstring: DynamicStrings.String;
    end_number: CARDINAL;
    prefix: STRING;
    suffix: STRING;

    current_number: CARDINAL;

    stdin_chan, stdout_chan, output_file_chan: ChanId;
    file_result: OpenResults;
BEGIN
    start_number_string := ReadLine("Start number: ");
    start_number := WholeConv.ValueCard(start_number_string);
    end_number_string := ReadLine("End number: ");
    end_number := WholeConv.ValueCard(end_number_string);
    prefix := ReadLine("Prefix: ");
    suffix := ReadLine("Suffix: ");

    Open(output_file_chan, "output.txt", write, file_result);

    FOR current_number := start_number TO end_number BY 1 DO
        WriteString(output_file_chan, prefix);
        WholeIO.WriteCard(output_file_chan, current_number, 0);
        WriteString(output_file_chan, suffix);
        WriteLn(output_file_chan);
    END;
    Flush(output_file_chan);
END Numgen.
