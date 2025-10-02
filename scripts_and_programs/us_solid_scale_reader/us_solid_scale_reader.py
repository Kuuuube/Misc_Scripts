import serial
import serial.tools.list_ports

ports = serial.tools.list_ports.comports()

for i, (port, desc, hwid) in enumerate(sorted(ports)):
    print("{}. {}: {} [{}]".format(i, port, desc, hwid))

SERIAL_PORT, _, _ = ports[int(input("Input serial port number: "))]
SERIAL_RATE = 9600
BYTE_SIZE = 8
STOPBITS = 1

ser = serial.Serial(port = SERIAL_PORT, baudrate = SERIAL_RATE, bytesize = BYTE_SIZE, stopbits = STOPBITS, timeout = 1)

while True:
    data = ser.readline()
    if (data != b'\r\n'):
        print(str(data).replace("b'\\x02","").replace("\\r\\n'", "").replace(" ", ""))
