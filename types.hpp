#include <type_traits>

// SI classes
template <class T = float>
struct SiBaseUnit { 
    static_assert(std::is_arithmetic<T>::value, "SiUnit must be a number");
    using Type = T; 
};

template <class T = float>
struct SiPrefix { 
    static_assert(std::is_arithmetic<T>::value, "SiUnit must be a number");
    using Type = T; 
};

template <class T = float> struct Meter_t   : SiBaseUnit<T> {};
template <class T = float> struct Second_t  : SiBaseUnit<T> {};
template <class T = float> struct Gram_t    : SiBaseUnit<T> {};  // should be KG? not sure how to handle this with prefixes...
template <class T = float> struct Ampere_t  : SiBaseUnit<T> {};
template <class T = float> struct Kelvin_t  : SiBaseUnit<T> {};
template <class T = float> struct Mole_t    : SiBaseUnit<T> {};
template <class T = float> struct Candela_t : SiBaseUnit<T> {};

template <class T = float> struct Tera  : SiPrefix<T> { static constexpr T value = 1e12; };
template <class T = float> struct Giga  : SiPrefix<T> { static constexpr T value = 1e9; };
template <class T = float> struct Mega  : SiPrefix<T> { static constexpr T value = 1e6; };
template <class T = float> struct Kilo  : SiPrefix<T> { static constexpr T value = 1e3; };
template <class T = float> struct Hecta : SiPrefix<T> { static constexpr T value = 1e2; };
template <class T = float> struct Deca  : SiPrefix<T> { static constexpr T value = 1e1; };
template <class T = float> struct Unit  : SiPrefix<T> { static constexpr T value = 1e0; };
template <class T = float> struct Deci  : SiPrefix<T> { static constexpr T value = 1e-1; };
template <class T = float> struct Centi : SiPrefix<T> { static constexpr T value = 1e-2; };
template <class T = float> struct Milli : SiPrefix<T> { static constexpr T value = 1e-3; };
template <class T = float> struct Micro : SiPrefix<T> { static constexpr T value = 1e-6; };
template <class T = float> struct Nano  : SiPrefix<T> { static constexpr T value = 1e-9; };
template <class T = float> struct Pico  : SiPrefix<T> { static constexpr T value = 1e-12; };

template <class T, template <class> class Prefix, class... BaseUnits> struct SiUnit;

namespace si_unit_details {

// Check if an object is an SI base unit type
template <class T>
struct IsSiBase : std::false_type {};
template <template <class> class Type, class T>
struct IsSiBase<Type<T>> : std::is_base_of<SiBaseUnit<T>, Type<T>> {};

// Helper for wrapping type lists
template <class...>
struct TypeList {};

// Join two lists of type classes
template <class...>
struct Join;
template <class... As, class... Bs>
struct Join<TypeList<As...>, TypeList<Bs...>> { using type = TypeList<As..., Bs...>; };
template <class... Ts>
using Join_t = typename Join<Ts...>::type;

// Logic AND for type lists
template<class...> struct conjunction : std::true_type { };
template<class B1> struct conjunction<B1> : B1 { };
template<class B1, class... Bn>
struct conjunction<B1, Bn...> : std::conditional_t<bool(B1::value), conjunction<Bn...>, B1> {};

// Check if a variadic list contains a type
template <class T, class...>
struct Contains : std::false_type {};

template <class T, class Head, class... Tail>
struct Contains<T, Head, Tail...> : std::conditional_t<std::is_same<T, Head>::value, std::true_type, Contains<T, Tail...>> {}; 

template <class... Ts>
constexpr bool Contains_v = Contains<Ts...>::value;

// Remove a type from a type list, if present
template <class T, class...>
struct Remove { using type = TypeList<>; };

template <class T, class Head, class... Tail>
struct Remove<T, Head, Tail...> {
    using type = std::conditional_t<
        std::is_same<T, Head>::value, 
            TypeList<Tail...>, 
            Join_t<TypeList<Head>, typename Remove<T, Tail...>::type>
    >;
};

template <class... Ts>
using Remove_t = typename Remove<Ts...>::type;

// Check if a variadic list contains a list of types
template <class...>
struct ContainsAll : std::false_type {};

template <class... Ts>
struct ContainsAll<TypeList<>, TypeList<Ts...>> : std::true_type {};

template <class Head, class... Tail, class... Ts>
struct ContainsAll<TypeList<Head, Tail...>, TypeList<Ts...>> : 
    conjunction<Contains<Head, Ts...>, ContainsAll<TypeList<Tail...>, Remove_t<Head, Ts...>>> {}; 

template <class... Ts>
constexpr bool ContainsAll_v = ContainsAll<Ts...>::value;

// Check if two type lists contain the elements
template <class, class>
struct EquivalentSet : std::false_type{};

template <class... As, class... Bs>
struct EquivalentSet<TypeList<As...>, TypeList<Bs...>>
{
    static constexpr bool value = 
        ContainsAll_v<TypeList<As...>, TypeList<Bs...>> &&
        ContainsAll_v<TypeList<Bs...>, TypeList<As...>>;
};

template <class... Ts>
constexpr bool EquivalentSet_v = EquivalentSet<Ts...>::value;

// type wrapper for specifying the inverse of the unit (1 / T)
// TODO: unit tests for Inverse(Inverse(T))
template <class T>
struct InverseTag {
    // TODO: this assert can fail due to losing the arithmetic type of T? the final types work without it though...
    static_assert(IsSiBase<T>::value, "can only invert SI base units");
};

template <class T>
struct Invert { using type = InverseTag<T>; };

template <class T>
struct Invert<InverseTag<T>> { using type = T; };

template <class T>
using Invert_t = typename Invert<T>::type;

template <class...>
struct TypeProduct { using type = TypeList<>; };

template <class Multiplier, class... Multiplicands>
struct TypeProduct<Multiplier, Multiplicands...> {
    using type = std::conditional_t<
        Contains_v<Invert_t<Multiplier>, Multiplicands...>,
            Remove_t<Invert_t<Multiplier>, Multiplicands...>,
            Join_t<TypeList<Multiplier>, TypeList<Multiplicands...>>
    >;
};

// Convert a TypeList to a SiUnit
template <class, template <class> class, class>
struct MakeSiUnit;

template <class T, template <class> class P, class... As>
struct MakeSiUnit<T, P, TypeList<As...>> { using type = SiUnit<T, P, As...>; };

template <class T, template <class> class P, class... Ts>
using MakeSiUnit_t = typename MakeSiUnit<T, P, Ts...>::type;

template <class...>
struct SiUnitProduct;

template <class T, template <class> class P, template <class> class Q, class... Multiplicands>
struct SiUnitProduct<SiUnit<T, P>, SiUnit<T, Q, Multiplicands...>> {
    using type = SiUnit<T, P, Multiplicands...>;  // TODO: compute Prefix somehow. it's not P
};

template <class T, template <class> class P, template <class> class Q, class Multiplier, class... Tail, class... Multiplicands>
struct SiUnitProduct<SiUnit<T, P, Multiplier, Tail...>, SiUnit<T, Q, Multiplicands...>> {
    using type = typename SiUnitProduct<SiUnit<T, P, Tail...>, // TODO: does the Prefix need cancelation here?
            MakeSiUnit_t<T, P, typename TypeProduct<Multiplier, Multiplicands...>::type>
    >::type;
};

template <class... Ts>
using SiUnitProduct_t = typename SiUnitProduct<Ts...>::type;

} // si_unit_details

template <class T, template <class> class Prefix, class... BaseUnits>
struct SiUnit {
    constexpr SiUnit() = default;
    constexpr SiUnit(T val) : value{val} {}

    template <template <class> class OtherPrefix, class... Others, std::enable_if_t<
                    si_unit_details::EquivalentSet_v<si_unit_details::TypeList<BaseUnits...>, 
                                                     si_unit_details::TypeList<Others...>>
                >* = nullptr>
    SiUnit(SiUnit<T, OtherPrefix, Others...> other) : value{other.value * other.prefix / prefix} {}

    constexpr SiUnit& operator=(T const& val) {
        value = val;
        return *this;
    }

    T value;
    static constexpr T prefix = Prefix<T>::value;
};



// Declare base types
using Scalar = SiUnit<float, Unit>;
using Meter  = SiUnit<float, Unit, Meter_t<float>>;
using Second = SiUnit<float, Unit, Second_t<float>>;
using Gram   = SiUnit<float, Unit, Gram_t<float>>;
using Kilogram = SiUnit<float, Kilo, Gram_t<float>>;
using Centimeter = SiUnit<float, Centi, Meter_t<float>>;

static_assert(sizeof(Gram) == sizeof(float), "");

// Multiplication
template <class T, template <class> class P, template <class> class Q, class... As, class... Bs>
constexpr auto operator*(SiUnit<T, P, As...> lhs, SiUnit<T, Q, Bs...> rhs) 
{
    using namespace si_unit_details;
    return SiUnitProduct_t<SiUnit<T, P, As...>, SiUnit<T, Q, Bs...>>{lhs.value * rhs.value};
}

template <class T, template <class> class P, class U, class... As>
constexpr auto operator*(SiUnit<T, P, As...> lhs, U rhs)  // TODO: use scalar types
{
    return SiUnit<T, P, As...>{lhs.value * rhs};
}

template <class T, template <class> class Q, class U, class... Bs>
constexpr auto operator*(T lhs, SiUnit<U, Q, Bs...> rhs) 
{
    return SiUnit<U, Q, Bs...>{lhs * rhs.value};
}

// Division
template <class T, template <class> class P, template <class> class Q, class... As, class... Bs>
constexpr auto operator/(SiUnit<T, P, As...> lhs, SiUnit<T, Q, Bs...> rhs) 
{
    using namespace si_unit_details;
    return SiUnitProduct_t<SiUnit<T, P, As...>, SiUnit<T, Q, Invert_t<Bs>...>>{lhs.value / rhs.value};
}

// Addition
template <class T, template <class> class P, template <class> class Q, class... As, class... Bs>
constexpr auto operator+(SiUnit<T, P, As...> lhs, SiUnit<T, Q, Bs...> rhs)
{
    using namespace si_unit_details;
    static_assert(EquivalentSet_v<TypeList<As...>, TypeList<Bs...>>, 
        "Addition can only be performed for SI types with equivalent unit definitions");
    return SiUnit<T, P, As...>{lhs.value + rhs.value};   // TODO: calculate P
}

// Subtraction
template <class T, template <class> class P, template <class> class Q, class... As, class... Bs>
constexpr auto operator-(SiUnit<T, P, As...> lhs, SiUnit<T, Q, Bs...> rhs)
{
    using namespace si_unit_details;
    static_assert(EquivalentSet_v<TypeList<As...>, TypeList<Bs...>>, 
        "Subtraction can only be performed for SI types with equivalent unit definitions");
    return SiUnit<T, P, As...>{lhs.value - rhs.value};  // TODO: calculate P
}

// Common Unit declarations
using MeterSq = decltype(Meter{} * Meter{});
using Newton = decltype(Kilogram{} * Meter{} / Second{} / Second{});

using MetersPerSecond = decltype(Meter{} / Second{});
using MetersPerSecondSq = decltype(Meter{} / Second{} / Second{});
using MeterSecond =decltype(Meter{} * Second{});
using Hertz = decltype(Scalar{} / Second{});

#include <iostream>

int main() {
    // Literals
    Meter l = 6;
    Meter w = 10.F;
    Scalar two = 2.F;
    Second minute = 60.;

    // Products
    Meter l2 = l * 2;
    Meter w2 = w * two;

    MeterSq area = l * w;
    MeterSq area2 = 200;
    Meter perimeter = 2 * l + 2 * w;

    // Unit Canceling / Division
    Scalar count = minute * Hertz{5};
    Scalar unitless = l / w;
    Meter cancel = l * l / w;
    MetersPerSecondSq g = 9.81F;
    Newton force = 100.;
    Gram mass = force / g;
    Kilogram mass_kg = force / g;

    std::cout << "mass = " << mass.value << " g" << std::endl;
    std::cout << "mass = " << mass_kg.value << " kg" << std::endl;

    // Out of order type conversions
    MeterSecond ms = minute * l;
    ms = l * minute;
    MeterSecond mixedSubtract = minute * l - l * minute;

    // FAIL TO COMPILE
    // Scalar x = l * l / w;
    // Meter a = l * w; 
    // Second s = l * w; 
    // Gram m = g / force;

    // Prefix conversion
    Gram sm = 1024.;
    Kilogram kg = sm;

    // std::cout << kg.value << std::endl;

    return 0;
}

