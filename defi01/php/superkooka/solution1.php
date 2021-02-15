<?php
/*
 * Rien à dire de particulier, effectivement c'est un algo basique, mais il fonctionne donc pas de soucis, Après si ça t'intéresse tu peux chercher sur les internets 2.0
 * D'autres type de triage, notamment ceux basés sur un système de nodes (non pas js :p)
 */
require_once __DIR__ . '/assertions.php';

fwrite(STDOUT, '-----' . PHP_EOL);
fwrite(STDOUT, 'Elephant road to the moon 🐘🚀🌕' . PHP_EOL);
fwrite(STDOUT, '-----' . PHP_EOL . PHP_EOL);
fwrite(STDOUT, '/!\ Cette solution prend 3 minutes pour la dernière assertion, let\'s go aller chercher un truc à boire :p' . PHP_EOL . PHP_EOL);


function getMinElementKey(array $array, int $n, int $offset): int
{
    $smallPosition = $offset;
    for ($i = $offset; $i < $n; $i++) {
        if ($array[$i] < $array[$smallPosition]) {
            $smallPosition = $i;
        }
    }
    return $smallPosition;
}

function tri(array $array, int $n): array
{
    for ($i = 0; $i < $n; $i++) {
            $smallerPosition = getMinElementKey($array, $n, $i);
            $el1 = $array[$i];
            $el2 = $array[$smallerPosition];

            $array[$i] = $el2;
            $array[$smallerPosition] = $el1;
    }

    return $array;
}

//code dupliqué dans les 2 fichiers, pareil c'est du chipotage (et mon phpstorm est pas content :p)
foreach (getAssertions() as $assertion) {
    //begin time
    $start_time = hrtime(true);

    try {
        assert($assertion['sorted_array'] === tri($assertion['array'], $assertion['size']));
        $end_time = hrtime(true);
        $eta = $end_time - $start_time;
        $eta = number_format($eta/1e+6);
        fwrite(STDOUT, "Asserting {$assertion['description']} in " . $eta . " ms" . PHP_EOL);
    } catch (AssertionError $e) {
        fwrite(STDOUT, "Failed to assert ${assertion['description']} 🛑");
        exit;
    }
}

fwrite(STDOUT, '-----' . PHP_EOL);
fwrite(STDOUT, 'Toutes les assertions passe avec succès ✅');
