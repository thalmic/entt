#ifndef ENTT_ENTITY_POLICY_HPP
#define ENTT_ENTITY_POLICY_HPP


#include <tuple>
#include <type_traits>
#include "sparse_set.hpp"


namespace entt {


/**
 * @brief Forward declaration of the registry class.
 */
template<typename>
class registry;


/**
 * TODO
 *
 * policy/signature model
 *
 * parts: induce/where/split/respect
 *
 * aspects:
 * - induce
 * - induce/where
 * - induce/where/split
 * - induce/where/respect
 * - induce/split
 * - induce/respect
 */


/**
 * @brief TODO
 *
 * TODO
 *
 * @tparam Entity TODO
 * @tparam Component TODO
 */
template<typename Entity, typename... Component>
class policy {
    static_assert(sizeof...(Component));

    /*! @brief A registry is allowed to create views. */
    friend class registry<Entity>;

    template<typename Comp>
    using pool_type = std::conditional_t<std::is_const_v<Comp>, const sparse_set<Entity, std::remove_const_t<Comp>>, sparse_set<Entity, Comp>>;

    template<typename Comp>
    using component_iterator_type = decltype(std::declval<pool_type<Comp>>().begin());

    template<typename Comp>
    using get_fn_type = Comp &(component_iterator_type<Comp> &, pool_type<Comp> &);

    using offset_type = typename sparse_set<Entity>::size_type;

    policy(sparse_set<Entity> *direct, pool_type<Component> *... pools, get_fn_type<Component> *... getters, offset_type base, offset_type length) ENTT_NOEXCEPT
        : direct{direct},
          pools{pools...},
          getters{getters...},
          base{base},
          length{length}
    {}

public:
    /*! @brief TODO */
    using entity_type = typename sparse_set<Entity>::entity_type;
    /*! @brief TODO */
    using size_type = typename sparse_set<entity_type>::size_type;
    /*! @brief TODO */
    using iterator_type = typename sparse_set<entity_type>::iterator_type;

    /*! @brief TODO */
    policy(const policy &) = default;
    /*! @brief TODO */
    policy(policy &&) = default;

    /*! @brief TODO @return TODO */
    policy & operator=(const policy &) = default;
    /*! @brief TODO @return TODO */
    policy & operator=(policy &&) = default;

    /**
     * @brief TODO
     *
     * TODO
     *
     * @return TODO
     */
    size_type size() const ENTT_NOEXCEPT {
        return length;
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @return TODO
     */
    bool empty() const ENTT_NOEXCEPT {
        return length;
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @return TODO
     */
    const entity_type * data() const ENTT_NOEXCEPT {
        return direct->data() + direct->size() - base - length;
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @return TODO
     */
    iterator_type begin() const ENTT_NOEXCEPT {
        return direct->begin() + base;
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @return TODO
     */
    iterator_type end() const ENTT_NOEXCEPT {
        return direct->begin() + base + length;
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @param entity TODO
     * @return TODO
     */
    iterator_type find(const entity_type entity) const ENTT_NOEXCEPT {
        const auto it = direct->find(entity);
        const auto from = begin();
        const auto to = end();
        return (it >= from && it < to && *it == entity) ? it : to;
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @param pos TODO
     * @return TODO
     */
    entity_type operator[](const size_type pos) const ENTT_NOEXCEPT {
        return (direct->begin()+base)[pos];
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @param entity TODO
     * @return TODO
     */
    bool contains(const entity_type entity) const ENTT_NOEXCEPT {
        return !(find(entity) == end());
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @tparam Comp TODO
     * @param entity TODO
     * @return TODO
     */
    template<typename... Comp>
    std::conditional_t<sizeof...(Comp) == 1, std::tuple_element_t<0, std::tuple<Comp &...>>, std::tuple<Comp &...>>
    get([[maybe_unused]] const entity_type entity) const ENTT_NOEXCEPT {
        assert(contains(entity));

        if constexpr(sizeof...(Comp) == 1) {
            static_assert(std::disjunction_v<std::is_same<Comp..., Component>..., std::is_same<std::remove_const_t<Comp>..., Component>...>);
            return (std::get<pool_type<Comp> *>(pools)->get(entity), ...);
        } else {
            return std::tuple<Comp &...>{get<Comp>(entity)...};
        }
    }

    /**
     * @brief TODO
     *
     * TODO
     *
     * @tparam Func TODO
     * @param func TODO
     * @return TODO
     */
    template<typename Func>
    inline void each(Func func) const {
        auto raw = std::make_tuple((std::get<pool_type<Component> *>(pools)->begin()+base)...);

        if constexpr(std::is_invocable_v<Func, std::add_lvalue_reference_t<Component>...>) {
            for(auto i = base, last = base + length; i < last; ++i) {
                func(std::get<get_fn_type<Component> *>(getters)(*(std::get<component_iterator_type<Component>>(raw)++), *std::get<pool_type<Component> *>(pools))...);
            }
        } else {
            const auto end = direct->begin() + base + length;
            auto begin = direct->begin() + base;

            while(begin != end) {
                func(*(begin++), std::get<get_fn_type<Component> *>(getters)(*(std::get<component_iterator_type<Component>>(raw)++), *std::get<pool_type<Component> *>(pools))...);
            }
        }
    }

private:
    sparse_set<entity_type> *direct;
    std::tuple<pool_type<Component> *...> pools;
    std::tuple<get_fn_type<Component> *...> getters;
    offset_type base;
    offset_type length;
};


}


#endif // ENTT_ENTITY_POLICY_HPP
