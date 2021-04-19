#!/usr/bin/env julia

using Random
using Base.Iterators

abstract type State end

struct YoungF <: State end # Young Forest
struct OldF   <: State end # Old Forest
struct FireE  <: State end # Early Fire
struct FireM  <: State end # Mid Fire
struct FireL  <: State end # Late Fire
struct Cinder <: State end # No forest, everything has been burn !
struct Border <: State end # Grid Border

function init(dims)
    world = Array{State, 2}(undef, dims...)

    for i in 1:dims[1], j in 1:dims[2]
        if i == 1 || i == dims[1] || j == 1 || j == dims[2]
            world[i,j] = Border()
        else
            world[i,j] = Cinder()
        end
    end

    return world
end

# Unicode pretty-printing
Base.show(io::IO, x::YoungF) =  print(io, '\U1F331')
Base.show(io::IO, x::OldF)   =  print(io, '\U1F332')
Base.show(io::IO, x::FireE)  =  print(io, '\U1F4A5')
Base.show(io::IO, x::FireM)  =  print(io, '\U1F525')
Base.show(io::IO, x::FireL)  =  print(io, '\U1F9EF')
Base.show(io::IO, x::Cinder) =  print(io, '\U1F480')
Base.show(io::IO, x::Border) =  print(io, "")

## Ansi colors pretty-printing
# Base.show(io::IO, x::YoungF) =  print(io, "\033[102m \033[0m")
# Base.show(io::IO, x::OldF)   =  print(io, "\033[42m \033[0m")
# Base.show(io::IO, x::FireE)  =  print(io, "\033[43m \033[0m")
# Base.show(io::IO, x::FireM)  =  print(io, "\033[101m \033[0m")
# Base.show(io::IO, x::FireL)  =  print(io, "\033[41m \033[0m")
# Base.show(io::IO, x::Cinder) =  print(io, " ")
# Base.show(io::IO, x::Border) =  print(io, "\033[100m \033[0m")


function evolve!(world)
    save = copy(world)
    n, m = size(world)

    for i in 2:(n-1), j in 2:(m-1)
        on_fE = false
        on_fM = false
        on_fL = false
        oldF = 0

        for k in (i-1):(i+1), l in (j-1):(j+1)
            if save[k,l] == OldF()
                oldF += 1
            elseif save[k,l] == FireE()
                on_fE = true
            elseif save[k,l] == FireM()
                on_fM = true
            elseif save[k,l] == FireL()
                on_fL = true
            end
        end

        if save[i,j] == Cinder()
            rand() ≤ 0.001 && (world[i,j] = YoungF())
        elseif save[i,j] == FireE()
            rand() ≤ 0.1 && (world[i,j] = FireM())
        elseif save[i,j] == FireM()
            rand() ≤ 0.1 && (world[i,j] = FireL())
        elseif save[i,j] == FireL()
            rand() ≤ 0.1 && (world[i,j] = Cinder())
        elseif save[i,j] == YoungF()
            if on_fE
                rand() ≤ 0.01 && (world[i,j] = FireE())
            elseif on_fM
                rand() ≤ 0.02 && (world[i,j] = FireE())
            elseif on_fL
                rand() ≤ 0.01 && (world[i,j] = FireE())
            else
                rand() ≤ 0.005 && (world[i,j] = OldF())
            end

        elseif save[i,j] == OldF()
            if on_fE
                rand() ≤ 0.1 && (world[i,j] = FireE())
            elseif on_fM
                rand() ≤ 0.2 && (world[i,j] = FireE())
            elseif on_fL
                rand() ≤ 0.1 && (world[i,j] = FireE())
            elseif oldF ≥ 6
                rand() ≤ 0.00005 && (world[i,j] = FireE())
            end
        end
    end
end

function draw(world)
    i, j = size(world)
    for k in 1:i
        for l in 1:j
            print("$(world[k,l])")
        end
        print("\n")
    end 
end

function play!(world, fps)
    n = 0
    try
        while true
            n += 1
            println("Generation: $n")
            evolve!(world)
            draw(world)
            sleep(1/fps)
        end
    catch ex
        if isa(ex, InterruptException)
            return n
        end
    end
end

function main()
    dims = (20,60)
    world = init(dims)
    fps = 10
    gen = play!(world, fps)
    println("Exited at generation $gen")
end

ccall(:jl_exit_on_sigint, Nothing, (Cint,), 0)
main()
