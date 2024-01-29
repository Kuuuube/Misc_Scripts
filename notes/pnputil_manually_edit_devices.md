List all device drivers:
`pnputil /enum-drivers`

List connected devices:
`pnputil /enum-devices /connected /drivers`



Remove a specific device driver:
`pnputil /delete-driver {published name} /uninstall /force /reboot`

Example:
`pnputil /delete-driver oem27.inf /uninstall /force /reboot`