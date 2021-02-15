<?php
// File created by Mjöllnir#3515 for testing

//require_once 'sol_7.4.php';
require_once 'sol_8.0.php';

class TestsCollection extends AbstractTestsCollection {

    public static function testOrderedIntArray2Pow10(): bool {
        $list = self::generateOrderedIntArrayOfGivenSize(2**10);
        return self::evaluateSorting($list);
    }

    public static function testReversedIntArray2Pow10(): bool {
        $list = self::generateReversedIntArrayOfGivenSize(2**10);
        return self::evaluateSorting($list);
    }

    public static function testOrderedIntArray2Pow15(): bool {
        $list = self::generateOrderedIntArrayOfGivenSize(2**15);
        return self::evaluateSorting($list);
    }

    public static function testReversedIntArray2Pow15(): bool {
        $list = self::generateReversedIntArrayOfGivenSize(2**15);
        return self::evaluateSorting($list);
    }

    public static function testOrderedIntArray2Pow16(): bool {
        $list = self::generateOrderedIntArrayOfGivenSize(2**16);
        return self::evaluateSorting($list);
    }

    public static function testReversedIntArray2Pow16(): bool {
        $list = self::generateReversedIntArrayOfGivenSize(2**16);
        return self::evaluateSorting($list);
    }

    public static function testOrderedIntArray2Pow17(): bool {
        $list = self::generateOrderedIntArrayOfGivenSize(2**17);
        return self::evaluateSorting($list);
    }

    public static function testReversedIntArray2Pow17(): bool {
        $list = self::generateReversedIntArrayOfGivenSize(2**17);
        return self::evaluateSorting($list);
    }

    public static function testOrderedIntArray2Pow20(): bool {
        $list = self::generateOrderedIntArrayOfGivenSize(2**20);
        return self::evaluateSorting($list);
    }

    public static function testReversedIntArray2Pow20(): bool {
        $list = self::generateReversedIntArrayOfGivenSize(2**20);
        return self::evaluateSorting($list);
    }
}

TestsCollection::run();
