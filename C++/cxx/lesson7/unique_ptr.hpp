#ifndef UTIL_UNIQUE_PTR_HPP
#define UTIL_UNIQUE_PTR_HPP

#include <utility>

namespace util {

namespace pseudostd {

template <typename T>
T&& move(T& x)
{
    return static_cast<T&&>(x);
}

template <typename T>
void swap(T& lhs, T& rhs)
{
    auto aux = move(lhs);
    lhs = move(rhs);
    rhs = move(aux);
}

}

template <typename T>
class unique_ptr {
public:
    unique_ptr():
        ptr{nullptr}
    {}

    unique_ptr(T* p):
        ptr{p}
    {}

    unique_ptr(const unique_ptr&) = delete;

    template <typename U>
    friend class unique_ptr;

    template<typename U>
    unique_ptr(unique_ptr<U>&& other):
        ptr{other.release()}
    {}

    unique_ptr& operator=(const unique_ptr&) = delete;

    // O operador de atribuição de move está implementado em separado
    // para servir como exemplo do idiom move-and-swap visto em aula.
    // Ele não permite, porém, mover um unique_ptr de tipo diferente,
    // mesmo que compatível:
    //
    // auto a = unique_ptr<Shape>{};
    // auto b = unique_ptr<Circle>{};
    //
    // a = move(b);
    //
    // Não funciona com essa implementação, pois os tipos são diferentes.
    unique_ptr& operator=(unique_ptr&& other)
    {
        auto aux = pseudostd::move(other);

        swap(*this, aux);

        return *this;
    }

    // Essa implementação permite o assignment com outros tipos de unique_ptr,
    // desde que o ponteiro interno seja compatível. Por causa disso, não é
    // possível utilizar move-and-swap. Acompanhe:
    //
    // auto a = unique_ptr<Shape>{};
    // auto b = unique_ptr<Circle>{};
    //
    // a = move(b);
    //
    // a.ptr possui tipo Shape*. b.ptr possui tipo Circle*. Não podemos,
    // portanto, utilizar `swap`, pois um `Circle*` não pode conter um
    // `Shape*` qualquer (ainda que o contrário seja possível.
    //
    // Assim sendo, foram implementadas as funções `reset` e `release`,
    // (baseadas no `std::unique_ptr`). Como o retorno de `std::release()`
    // e o parâmetro de `std::reset()` são ponteiros crus, a conversão entre
    // tipos compatíveis ocorre normalmente.
    template <typename U>
    unique_ptr& operator=(unique_ptr<U>&& other)
    {
        reset(other.release());

        return *this;
    }

    friend void swap(unique_ptr& lhs, unique_ptr& rhs)
    {
        using pseudostd::swap; // enable ADL/König lookup

        swap(lhs.ptr, rhs.ptr);  // find swap by ADL
    }

    ~unique_ptr()
    {
        delete ptr;
    }

    T& operator*()
    {
        return *this->ptr;
    }

    T* operator->()
    {
        return this->ptr;
    }

    // `reset()` faz com que o `unique_ptr` aponte para um novo ponteiro,
    //
    // Isso garante a destruição do conteúdo anterior, delegando a gerência do
    // antigo para um `unique_ptr` local ao escopo da função `reset()`.
    void reset(T* p = nullptr)
    {
        auto aux = unique_ptr{p};
        swap(*this, aux);
    }

    // `release()` abdica da gerência do ponteiro interno, retornando-o.
    //
    // **Uma chamada sozinha para `release()` equivale a vazar o conteúdo do
    // ponteiro.** `release` deve ser utilizado para entregar a gerência do
    // ponteiro interno para outra entidade.
    T* release()
    {
        using pseudostd::swap;

        T* p = nullptr;
        swap(p, this->ptr);
        return p;
    }

private:
    T* ptr;
};


template <typename T, typename ...Args>
unique_ptr<T> make_unique(Args&& ...args)
{
    return unique_ptr<T>{ new T{std::forward<Args>(args)...} };
}


}

#endif
