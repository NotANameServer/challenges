<?php declare(strict_types=1);

/**
 * PHP 7.4 version
 *
 * Sort an array of numbers where each is between (PHP_INT_MIN / 2) AND (PHP_INT_MAX / 2)
 * Based on the tree sort algorithm
 *
 * @param int[] $a
 * @return int[]
 *
 * @example $sortedList = sortingChallenge([4, 5, 1, 3, 9, 6, 7, 8, 2, -1], 10);
 */
function sortingChallenge($a, int $n)
{
    return (new IntTreeSort($a, $n))->getList();
}

class IntNode
{
    public int $value;

    public ?IntNode $parent;

    public ?IntNode $higher;

    public ?IntNode $lower;

    public function __construct(int $value, ?IntNode $parent=null)
    {
        $this->value = $value;
        $this->parent = $parent;
    }
}

class IntTree
{
    private IntNode $root;

    public function getRootNode(): ?IntNode
    {
        return $this->root ?? null;
    }

    public function addValue(int $value): void
    {
        if ($this->isTreeEmpty())
            $this->generateRootNode($value);
        else
            $this->generateNode($value);
    }

    private function isTreeEmpty(): bool
    {
        return isset($this->root) === false;
    }

    private function generateRootNode(int $value): void
    {
        $this->root = new IntNode($value);
    }

    private function generateNode(int $value): void
    {
        $node = $this->getRootNode();

        $direction = $this->getLinkDirection($node, $value);
        while (isset($node->{$direction}))
        {
            $node = $node->{$direction};
            $direction = $this->getLinkDirection($node, $value);
        }

        $this->attachNodeBasedOnValue($node, $value, $direction);
    }

    private function attachNodeBasedOnValue(IntNode $node, int $value, string $direction): void
    {
        $node->{$direction} = new IntNode($value, $node);
    }

    private function getLinkDirection(IntNode $node, int $value): string
    {
        return ($node->value < $value) ? 'higher' : 'lower';
    }
}

class IntTreeSort
{
    /**
     * @var int[]
     */
    private $list;

    private int $size;

    private int $indexToTheNextValueToOrder = 0;

    /**
     * @var IntTree[]
     */
    private SplFixedArray $treeBuckets;

    private int $minListValue;

    private int $intervalPerBucket;

    /**
     * @param int[] $list
     */
    public function __construct($list, int $size)
    {
        $this->list = $list;
        $this->size = $size;

        if ($this->canBeSorted())
            $this->sort();
    }

    private function canBeSorted(): bool
    {
        return isset($this->list[2]);
    }

    private function sort(): void
    {
        $this->prepareTreeBuckets();
        $this->fetchListValuesInTreeBucket();
        $this->computeTrees();
    }

    private function prepareTreeBuckets(): void
    {
        $bucketSize = $this->getTreeBucketSize();
        $this->createTreeBuckets($bucketSize);
        $this->instantiateBucketParamsBasedOnSize($bucketSize);
    }

    private function getTreeBucketSize(): int
    {
        return (int) sqrt($this->size);
    }

    private function createTreeBuckets(int $size): void
    {
        $treeBuckets = new SplFixedArray($size);
        for ($i = 0; $i < $size; $i++)
        {
            $treeBuckets[$i] = new IntTree();
        }
        $this->treeBuckets = $treeBuckets;
    }

    private function fetchListValuesInTreeBucket(): void
    {
        foreach($this->list as $value)
        {
            $bucketIndex = (int) floor(($value - $this->minListValue) / $this->intervalPerBucket);
            $this->treeBuckets[$bucketIndex]->addValue($value);
        }
    }

    private function instantiateBucketParamsBasedOnSize(int $bucketSize): void
    {
        $min = $max = $this->list[0];
        for ($i = 1; $i < $this->size; $i++)
        {
            $value = $this->list[$i];
            if ($min > $value)  $min = $value;
            elseif ($max < $value)  $max = $value;
        }

        $this->minListValue = $min;
        /**
         * - The minus 1 is here to have an interval that corresponds to the size of the bucket
         *      ($index => ($size - 1) else "Index invalid or out of range"
         *
         * - Prevent division by 0 - fallback with `?: 1`
         *      Needs to be more analysed to avoid another side effects from this hack
         */
        $this->intervalPerBucket = (int) (abs($max - $min) / ($bucketSize - 1)) ?: 1;
    }

    private function computeTrees(): void
    {
        foreach($this->treeBuckets as $tree)
        {
            if ($root = $tree->getRootNode())
                $this->walkInTreeToSortList($root);
        }
    }

    private function walkInTreeToSortList(IntNode $node): void
    {
        $node = $this->walkUpTreeFromTheLowestNode($node);
        while ($node->parent !== null)
        {
            $node = $node->parent;
            if (isset($node->lower))
            {
                $this->setOrderedValue($node->value);
                $this->walkDownTreeByTheHigher($node);
            }
        }
    }

    private function walkDownTreeByTheHigher(IntNode $node): void
    {
        if (isset($node->higher))
        {
            $node->higher->parent = null;
            $this->walkInTreeToSortList($node->higher);
        }
    }

    private function walkUpTreeFromTheLowestNode(IntNode $node): IntNode
    {
        do
        {
            $node = $this->getLowermostNode($node);
            $this->setOrderedValue($node->value);

            if (isset($node->higher) === false)
                return $node;

            $node = $node->higher;
        }
        while (true); // CORRECTION pas fan des while(true)
    }

    private function getLowermostNode(IntNode $node): IntNode
    {
        while (isset($node->lower))
            $node = $node->lower;

        return $node;
    }

    private function setOrderedValue(int $value): void
    {
        $this->list[$this->indexToTheNextValueToOrder] = $value;
        $this->indexToTheNextValueToOrder++;
    }

    /**
     * @return int[]
     */
    public function getList()
    {
        return $this->list;
    }
}


/** *********************************************************************************
 * Part for the tests,can just be ignored but necessary to keep a minimum robustness to
 * the modifications. Theses tests aren't optimal and never intended to be.
 * *********************************************************************************
 * @example AbstractTestsCollection::run(); // entry point
 */

abstract class SkipTest {}

abstract class AbstractIntTreeSortTest
{
    public static function run()
    {
        $validTestCounter = 0;

        $testMethods = self::getTestMethods();
        foreach ($testMethods as $method)
        {
            $validTestCounter += (int) self::runTest($method);
        }

        self::displayResult($testMethods, $validTestCounter);
    }

    /**
     * @return string[]
     */
    private static function getTestMethods(): array
    {
        $testReflectionClass = new ReflectionClass(static::class);
        foreach ($testReflectionClass->getMethods() as $method)
        {
            if (self::isTestMethod($method->name))
                $testMethods[] = $method->name;
        }
        return $testMethods ?? [];
    }

    /**
     * @param ReflectionAttribute[] $attributes
     */
    private static function shouldSkipTest(array $attributes): bool
    {
        foreach ($attributes as $attr)
        {
            if ($attr->getName() === SkipTest::class)
            {
                return true;
            }
        }
        return false;
    }

    private static function isTestMethod(string $name): bool
    {
        return strpos($name, 'test') === 0;
    }

    private static function runTest($method): bool
    {
        AbstractDumbBenchmark::start();

        $isValid = static::$method();
        if ($isValid === false)
        {
            echo "The following test has failed {$method}.\n";
        }

        AbstractDumbBenchmark::end($method);

        return $isValid;
    }

    private static function displayResult(array $testMethods, int $validTestCounter): void
    {
        $nbTest = count($testMethods);
        echo "\n{$validTestCounter} / {$nbTest} test(s) has passed.\n";
        AbstractDumbBenchmark::resume();
    }

    /**
     * @param int[] $list
     */
    protected static function evaluateSorting($list): bool
    {
        $witness = $list instanceof SplFixedArray ? $list->toArray() : $list;
        $sorted = sortingChallenge($list, count($list));
        sort($witness);
        return self::isListInSameOrder($sorted, $witness);
    }

    /**
     * @param int[] $list1
     * @param int[] $list2
     */
    private static function isListInSameOrder($list1, $list2): bool
    {
        $size = count($list1);
        if ($size !== count($list2))
            return false;

        for ($i = 0; $i < $size; $i++)
        {
            if ($list1[$i] !== $list2[$i])
                return false;
        }

        return true;
    }

    /**
     * @return int[]
     */
    protected static function generateRandomIntArrayOfGivenSize(int $size): SplFixedArray
    {
        $list = new SplFixedArray($size);
        for ($i = 0; $i < $size; $i++)
        {
            $list[$i] = random_int(PHP_INT_MIN>>1, PHP_INT_MAX>>1);
        }
        return $list;
    }

    // CORRECTION method created for testing
    protected static function generateOrderedIntArrayOfGivenSize(int $size): SplFixedArray {
        $list = new SplFixedArray($size);

        for ($i = 0; $i < $size; $i++) {
            $list[$i] = $i;
        }

        return $list;
    }

    // CORRECTION method created for testing
    protected static function generateReversedIntArrayOfGivenSize(int $size): SplFixedArray {
        $list = new SplFixedArray($size);

        for ($i = $size - 1; $i >= 0; $i--) {
            $list[$i] = $i;
        }

        return $list;
    }
}

abstract class AbstractTestsCollection extends AbstractIntTreeSortTest
{
    public static function testEmptyArray(): bool
    {
        return self::evaluateSorting([]);
    }

    public static function testDefaultArray(): bool
    {
        $list = [4, 5, 1, 3, 9, 6, 7, 8, 2, -1, 0];
        return self::evaluateSorting($list);
    }

    public static function testRandomArray2Pow10(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(2**10);
        return self::evaluateSorting($list);
    }

    public static function testRandomArray2Pow15(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(2**15);
        return self::evaluateSorting($list);
    }

    public static function testRandomArray2Pow16(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(2**16);
        return self::evaluateSorting($list);
    }

    public static function testRandomArray2Pow17(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(2**17);
        return self::evaluateSorting($list);
    }

    public static function testRandomArray2Pow20(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(2**20);
        return self::evaluateSorting($list);
    }

    #[SkipTest("a little long")]
    public static function skip_testRandomArray100000(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(100000);
        return self::evaluateSorting($list);
    }

    #[SkipTest("memory overflow")]
    public static function skip_testRandomArrayMaxInt(): bool
    {
        $list = self::generateRandomIntArrayOfGivenSize(PHP_INT_MAX>>1);
        return self::evaluateSorting($list);
    }
}

abstract class AbstractDumbBenchmark
{
    public static bool $displayBench = true;

    public static float $benchStart; // CORRECTION changed from int to float type

    public static function start(): void
    {
        self::$benchStart = hrtime(true);
    }

    public static function end(string $name='test'): void
    {
        $time = hrtime(true) - self::$benchStart; // CORRECTION petit bémole ici, je teste ce script sur un raspberryPI qui est en 32 bits, par conséquent, hrtime() me retourne un float -> Fatal Error
        $time = number_format($time / 1e+6);
        echo "{$name} run in {$time} ms\n\n";
    }

    public static function resume(): void
    {
        $memory = number_format(memory_get_peak_usage());
        echo "{$memory} maximum memory has been reached\n";
    }
}


/** *********************************************************************************
 * Call example
 * *********************************************************************************
 */

//Part for the test
//AbstractTestsCollection::run();

//Part for an usage example
/*
$list = [4, 5, 1, 3, 9, 6, 7, 8, 2, -1];
$size = count($list);

$sortedList = sortingChallenge($list, $size);

print_r($list);
print_r($sortedList);
*/
