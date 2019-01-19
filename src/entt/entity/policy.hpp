#ifndef ENTT_ENTITY_POLICY_HPP
#define ENTT_ENTITY_POLICY_HPP


#include "../core/type_traits.hpp"


namespace entt {


template<typename...>
struct policy;


template<typename... Induce, typename... Where, typename... Split, typename... Respect>
struct policy<type_list<Induce...>, type_list<Where...>, type_list<Split...>, type_list<Respect...>> {
    template<typename... Other>
    using induce = policy<type_list<Induce..., Other...>, type_list<Where...>, type_list<Split...>, type_list<Respect...>>;

    template<typename... Other>
    using where = policy<type_list<Induce...>, type_list<Where..., Other...>, type_list<Split...>, type_list<Respect...>>;

    template<typename... Other>
    using split = policy<type_list<Induce...>, type_list<Where...>, type_list<Split..., Other...>, type_list<Respect...>>;

    template<typename... Other>
    using respect = policy<type_list<Induce...>, type_list<Where...>, type_list<Split...>, type_list<Respect..., Other...>>;
};


template<typename... Induce>
using induce = policy<type_list<Induce...>, type_list<>, type_list<>, type_list<>>;


template<typename... Where>
using where = policy<type_list<>, type_list<Where...>, type_list<>, type_list<>>;


template<typename... Split>
using split = policy<type_list<>, type_list<>, type_list<Split...>, type_list<>>;


template<typename... Respect>
using respect = policy<type_list<>, type_list<>, type_list<>, type_list<Respect...>>;


}


#endif // ENTT_ENTITY_POLICY_HPP
