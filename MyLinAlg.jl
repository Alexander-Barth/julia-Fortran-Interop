module MyLinAlg

function mul!(C, A, B)
    C .= 0
    for j in axes(C, 2)
        for i in axes(C, 1)
            for k in axes(A, 2)
                C[i, j] += A[i, k] * B[k, j]
            end
        end
    end
    return
end

end
