# Anki Addon Dependencies

## Installing

Dependencies can be installed to a specific folder by using the `-t` flag. For example:

```
pip3 install requests -t ./lib
```

This will install the `requests` module to a folder named `lib`.

## Accessing

The module can now be imported in one of the following ways:

1. Adding lib folder to path (recommended):

    This method allows for dependency trees to work correctly. For example, if `requests` imports another dependency, it will have it and will not require any special handling. This would fail when directly importing as shown in methods 2 and 3.

    ```
    import sys
    # add lib folder to path to allow accessing modules there
    sys.path.append(sys.path[0] + "/lib")

    import requests
    ```

2. `from x import y`:

    ```
    from lib import requests
    ```

3. `import x.y`:

    ```
    import lib.requests
    ```

Run `setup_dependencies.sh` and `test.py` for a working example.
