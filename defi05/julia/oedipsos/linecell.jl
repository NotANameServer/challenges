#!/usr/bin/env julia

import Random

gen(n::Integer) = Random.rand(0:3, n)
trivial(n, i) = (a = zeros(Int, n); a[i] = 1; a)
show(world) = println("$world") 

function evolve!(world)
    n = length(world)
    old::Int = 0

    for i ∈ 1:n
        if i == 1
            s = sum(world[i:i+1])
        elseif i == n
            s = old + world[i]
        else
            s = old + sum(world[i:i+1])
        end

        old = world[i]

        s ∈ [0, 3, 4] &&
            (world[i] = 0; continue)
        s ∈ [5, 9] &&
            (world[i] = 1; continue)
        s ∈ [2, 7] &&
            (world[i] = 2; continue)
        s ∈ [1, 6, 8] &&
            (world[i] = 3; continue)
    end

end

function main()

    for n ∈ 2:100

        world = trivial(n, (n+1)÷2)
        # show(world)
        
        for i ∈ 1:1000
            old = copy(world)
            evolve!(world)
            if old == world # stationnary case
                println("Stopped at $i iterations (n = $n)")
                show(world)
                break
            end
        end 
    end
end

main()



