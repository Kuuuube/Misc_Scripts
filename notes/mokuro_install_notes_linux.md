# Mokuro with cuda using pipx on Linux

## Install cuda and pipx

I'm using arch so this is the arch install command, use whatever your distro has:

```
sudo pacman -S pipx python-pytorch-cuda python-torchvision-cuda
```

You could get away with just installing the base cuda package since pipx will install the pytorch python package but installing the pytorch packages ensures they have all system dependencies.

If your distro lacks pytorch packages you with want to install cuda separate instead.

## Verify cuda and pytorch installation

1. Get into the python shell:

    ```
    python
    ```

2. Check if torch can see cuda (it will print `True` if cuda is detected):

    ```
    import torch
    print(torch.cuda.is_available())
    exit()
    ```

## Install mokuro

```
pipx install mokuro
```

## Edit pytorch version inside pipx

These steps may be required for using very new GPUs. At the time of writing, nightly with cuda 12.8 is requried for nvidia 50 series.

1. Get the install command from https://pytorch.org/get-started/locally/ by selecting your requrired options.
2. Copy the command and replace everything before `torch` with `pipx inject mokuro` (add `--pip-args="--pre"` if installing a nightly build).
3. Add `--force` on the end to ensure old packages are overridden.

Example (normal build):
```
pipx inject mokuro torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu128 --force
```

Example (nightly build):
```
pipx inject mokuro torch torchvision --pip-args="--pre" --index-url https://download.pytorch.org/whl/nightly/cu128 --force
```

## Run mokuro

- Single folder:

    ```
    mokuro path/to/directory
    ```

- Many folders:

    ```
    mokuro --parent-dir path/to/directory
    ```

(Use `.` for current directory)
