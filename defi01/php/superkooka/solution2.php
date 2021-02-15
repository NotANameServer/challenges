<?php

/* Rien à dire à part du petit refacto, en pure algo c'est peut être un peu de la triche, mais en soit, pour le boulot c'est une bonne solution
*  d'utiliser ce que permet de faire un langage, le usort et tu dois quand même lui passer une fonction avec ce qu'il doit faire de tout façon
*/

require_once __DIR__ . '/assertions.php';

fwrite(STDOUT, '-----' . PHP_EOL);
fwrite(STDOUT, 'Elephant road to the moon 🐘🚀🌕' . PHP_EOL);
fwrite(STDOUT, '-----' . PHP_EOL . PHP_EOL);
fwrite(STDOUT, '/!\ Cette solution est potentiellement de la triche, j\'ai utiliser la fonction usort' . PHP_EOL . PHP_EOL);

// j'ai enlevé le paramètre en trop inutile (coucou le copier/coller :D)
function tri(array $array): array
{
    //il y a moyen de refacto le usort comme ça (c'est du chipotage)
    usort($array, static function ($a, $b) {
        if($a === $b){
            return 0;
        }

        return ($a > $b) ? 1 : -1;
    });

    /*usort($array, static function ($a, $b) {
        if ($a > $b) {
            return 1;
        }

        if ($a < $b) {
            return -1;
        }

        return 0;
    });*/

    return $array;
}

//code dupliqué dans les 2 fichiers, pareil c'est du chippotage (et mon phpstorm est pas content :p)
foreach (getAssertions() as $assertion) {
    $start_time = hrtime(true);

    try {
        assert($assertion['sorted_array'] === tri($assertion['array']));
        $end_time = hrtime(true);
        $eta = $end_time - $start_time;
        $eta = number_format($eta/1e+6);
        fwrite(STDOUT, "Asserting {$assertion['description']} in " . $eta . " ms" . PHP_EOL);
        $memory = number_format(memory_get_peak_usage());
        fwrite(STDOUT, "{$memory} maximum memory has been reached" . PHP_EOL);
    } catch (AssertionError $e) {
        fwrite(STDOUT, "Failed to assert ${assertion['description']} 🛑");
        exit;
    }
}

fwrite(STDOUT, '-----' . PHP_EOL);
fwrite(STDOUT, 'Toutes les assertions passe avec succès ✅');
