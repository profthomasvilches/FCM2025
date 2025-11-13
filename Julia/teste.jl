

using Random

#? Referencia ótima!
#? https://www.youtube.com/@doggodotjl

#* do repositorio do doggodotjl
#* https://github.com/julia4ta/tutorials/blob/master/Series%2006/Files/julia_cheatsheet.jl

#? Motivação

function check(x, v)
    x ∈ v
end

x = 2

v = rand(1:10000, 100000000);

@time check(23, v)