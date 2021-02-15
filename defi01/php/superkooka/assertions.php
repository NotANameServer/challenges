<?php

//rien à dire parfait
function getAssertions(): Generator
{
    $big_array_randomized = range(-32768, 32768);
    shuffle($big_array_randomized);

    yield [
        'array' => [],
        'size' => 0,
        'sorted_array' => [],
        'description' => 'empty array',
    ];

    yield [
        'array' => [3, 5, 4, 2, 1],
        'size' => 5,
        'sorted_array' => [1, 2, 3, 4, 5],
        'description' => 'small array',
    ];

    yield [
        'array' => [8, PHP_INT_MAX, 7, PHP_INT_MIN],
        'size' => 4,
        'sorted_array' => [PHP_INT_MIN, 7, 8, PHP_INT_MAX],
        'description' => 'small array with constant',
    ];

    yield [
        'array' => $big_array_randomized,
        'size' => 2 ** 16 + 1,
        'sorted_array' => range(-32768, 32768),
        'description' => 'big array',
    ];
}