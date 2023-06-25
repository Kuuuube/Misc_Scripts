How to find which program has opened a specific usb device:

1. In device manager find the device to look up

2. Open properties for the device

3. Go to the `Details` tab

4. Set the dropdown to `Physical Device Object name`

5. In process explorer go to `Find` > `Find Handle or DLL`

6. Enter in the name from device manager into the process explorer search. It should look something like `\Device\0000028e`



How to trace usb devices opened by a program with process explorer:

1. In process explorer go to `Find` > `Find Handle or DLL` then enter the name of a program if the program is known or `\Device\0` or `\Device\` if the program is unknown.

3. Find the desired device under the `Name` column

4. Enter the following in CMD replacing {Enter_PDO_Here} with the `Name` in process explorer. The name should be cut down to the last characters or have the slashes escaped. For example: `\Device\0000028e` should be entered as `0000028e` or `\\Device\\0000028e`.
`wmic path Win32_PnPSignedDriver where "pdo like '%{Enter_PDO_Here}'" get DeviceName,HardWareID, PDO`

5. Optionally, if the above command does not give enough information, try running the following command. It will return all possible parameters. You may need to copy it out of the terminal or pipe it to a file to properly view it.
`wmic path Win32_PnPSignedDriver where "pdo like '%{Enter_PDO_Here}'" get ClassGuid,CompatID,Description,DeviceClass,DeviceID,DeviceName,DevLoader,DriverDate,DriverName,DriverVersion,FriendlyName,HardWareID,InfName,InstallDate,IsSigned,Location,Manufacturer,Name,PDO,DriverProviderName,Signer,Started,StartMode,Status,SystemCreationClassName,SystemName`


Alternatively, run the following command to list all devices.
`wmic path Win32_PnPSignedDriver where "pdo like '%'" get DeviceName,HardWareID, PDO`

Listing full data for all devices is also possible with the following command:
`wmic path Win32_PnPSignedDriver where "pdo like '%'" get ClassGuid,CompatID,Description,DeviceClass,DeviceID,DeviceName,DevLoader,DriverDate,DriverName,DriverVersion,FriendlyName,HardWareID,InfName,InstallDate,IsSigned,Location,Manufacturer,Name,PDO,DriverProviderName,Signer,Started,StartMode,Status,SystemCreationClassName,SystemName`