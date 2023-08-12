# Non-branching One Dimensional Linear Interpolation

Methods for interpolating one-dimensional input between different scales with various static and non-static values.

`val_1` is the input and is contained between `min_val_1` and `max_val_1`. It is interpolated to be between `min_val_2` and `max_val_2`.

Bounds not marked with {anything} must be the exact value listed to function properly. Bounds marked with {anything} may contain any number.

Inputs outside the given min and max bounds may produce odd results. External filtering is required before running input through these.

```
min_val_1 = 0 max_val_1 = {anything}
min_val_2 = 0 max_val_2 = 1

val_1 / max_val_1
```

```
min_val_1 = 0 max_val_1 = {anything}
min_val_2 = {anything} max_val_2 = 1

(val_1 / (max_val_1 / min_val_2)) + min_val_2
```

```
min_val_1 = 0 max_val_1 = {anything}
min_val_2 = {anything} max_val_2 = {anything}

(val_1 / (max_val_1 / (max_val_2 - min_val_2))) + min_val_2
```

```
min_val_1 = {anything} max_val_1 = {anything}
min_val_2 = {anything} max_val_2 = {anything}

((val_1 - min_val_1) / ((max_val_1 - min_val_1) / (max_val_2 - min_val_2))) + min_val_2
```
