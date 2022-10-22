# EVE Online Targetting Calculator

Calculates how long it takes to lock target on a ship in [EVE Online](https://www.eveonline.com/).

## Usage

- Run either `targeting_calculator.au3` for calculation every time you press `OK` or `targeting_calculator_live_update.au3` for live updates as you type.

## Dependencies

Install [AutoIt](https://www.autoitscript.com) and run the scripts using `AutoIt3_x64.exe`.

## Notes

- I do not play EVE Online anymore.

- If you actually use this I would highly recommend remaking it in something besides AutoIt.

- The locktime formula is `lock_time = (40000 / scan_resolution) / (arcsinh(signature_radius) ^ 2)`. Scan resolution is your ship's scan resolution. Signature radius is the target ship's signature radius. 

    This is how the arcsinh part can be calculated if you don't want to/can't use math libraries `Log(signature_radius+(Sqrt(signature_radius^2+1))/Log(2.7182818284590452353))`