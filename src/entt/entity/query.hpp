#ifndef ENTT_ENTITY_QUERY_HPP
#define ENTT_ENTITY_QUERY_HPP


#include <iterator>
#include <cassert>
#include <cstddef>
#include <array>
#include <tuple>
#include <vector>
#include <utility>
#include <algorithm>
#include <type_traits>
#include "../config/config.h"
#include "entt_traits.hpp"
#include "sparse_set.hpp"


namespace entt {


/**
 * @brief Forward declaration of the registry class.
 */
template<typename>
class registry;


/**
 * @brief Query.
 *
 * Queries iterate over those entities that have at least all the given
 * components in their bags. During initialization, a query looks at the number
 * of entities available for each component and picks up a reference to the
 * smallest set of candidate entities in order to get a performance boost when
 * iterate.<br/>
 * Order of elements during iterations are highly dependent on the order of the
 * underlying data structures. See sparse_set and its specializations for more
 * details.
 *
 * @b Important
 *
 * Iterators aren't invalidated if:
 *
 * * New instances of the given components are created and assigned to entities.
 * * The entity currently pointed is modified (as an example, if one of the
 *   given components is removed from the entity to which the iterator points).
 *
 * In all the other cases, modifying the pools of the given components in any
 * way invalidates all the iterators and using them results in undefined
 * behavior.
 *
 * @note
 * Queries share references to the underlying data structures with the registry
 * that generated them. Therefore any change to the entities and to the
 * components made by means of the registry are immediately reflected by
 * queries.
 *
 * @warning
 * Lifetime of a query must overcome the one of the registry that generated it.
 * In any other case, attempting to use a query results in undefined behavior.
 *
 * @tparam Entity A valid entity type (see entt_traits for more details).
 * @tparam Component Types of components iterated by the query.
 */
template<typename Entity, typename... Component>
class query {
    static_assert(sizeof...(Component) > 1);

    /*! @brief A registry is allowed to create queries. */
    friend class registry<Entity>;

    template<typename Comp>
    using pool_type = std::conditional_t<std::is_const_v<Comp>, const sparse_set<Entity, std::remove_const_t<Comp>>, sparse_set<Entity, Comp>>;

    template<typename Comp>
    using component_iterator_type = decltype(std::declval<pool_type<Comp>>().begin());

    using underlying_iterator_type = typename sparse_set<Entity>::iterator_type;
    using unchecked_type = std::array<const sparse_set<Entity> *, (sizeof...(Component) - 1)>;
    using traits_type = entt_traits<Entity>;

    class iterator {
        friend class query<Entity, Component...>;

        using extent_type = typename sparse_set<Entity>::size_type;

        iterator(unchecked_type unchecked, underlying_iterator_type begin, underlying_iterator_type end) ENTT_NOEXCEPT
            : unchecked{unchecked},
              begin{begin},
              end{end},
              extent{min(std::make_index_sequence<unchecked.size()>{})}
        {
            if(begin != end && !valid()) {
                ++(*this);
            }
        }

        template<std::size_t... Indexes>
        extent_type min(std::index_sequence<Indexes...>) const ENTT_NOEXCEPT {
            return std::min({ std::get<Indexes>(unchecked)->extent()... });
        }

        bool valid() const ENTT_NOEXCEPT {
            const auto entity = *begin;
            const auto sz = size_type(entity & traits_type::entity_mask);

            return sz < extent && std::all_of(unchecked.cbegin(), unchecked.cend(), [entity](const sparse_set<Entity> *view) {
                return view->fast(entity);
            });
        }

    public:
        using difference_type = typename underlying_iterator_type::difference_type;
        using value_type = typename underlying_iterator_type::value_type;
        using pointer = typename underlying_iterator_type::pointer;
        using reference = typename underlying_iterator_type::reference;
        using iterator_category = std::forward_iterator_tag;

        iterator() ENTT_NOEXCEPT = default;

        iterator(const iterator &) ENTT_NOEXCEPT = default;
        iterator & operator=(const iterator &) ENTT_NOEXCEPT = default;

        iterator & operator++() ENTT_NOEXCEPT {
            return (++begin != end && !valid()) ? ++(*this) : *this;
        }

        iterator operator++(int) ENTT_NOEXCEPT {
            iterator orig = *this;
            return ++(*this), orig;
        }

        bool operator==(const iterator &other) const ENTT_NOEXCEPT {
            return other.begin == begin;
        }

        inline bool operator!=(const iterator &other) const ENTT_NOEXCEPT {
            return !(*this == other);
        }

        pointer operator->() const ENTT_NOEXCEPT {
            return begin.operator->();
        }

        inline reference operator*() const ENTT_NOEXCEPT {
            return *operator->();
        }

    private:
        unchecked_type unchecked;
        underlying_iterator_type begin;
        underlying_iterator_type end;
        extent_type extent;
    };

    // we could use pool_type<Component> *..., but vs complains about it and refuses to compile for unknown reasons (likely a bug)
    query(sparse_set<Entity, std::remove_const_t<Component>> *... pools) ENTT_NOEXCEPT
        : pools{pools...}
    {}

    const sparse_set<Entity> * candidate() const ENTT_NOEXCEPT {
        return std::min({ static_cast<const sparse_set<Entity> *>(std::get<pool_type<Component> *>(pools))... }, [](const auto *lhs, const auto *rhs) {
            return lhs->size() < rhs->size();
        });
    }

    unchecked_type unchecked(const sparse_set<Entity> *view) const ENTT_NOEXCEPT {
        unchecked_type other{};
        std::size_t pos{};
        ((std::get<pool_type<Component> *>(pools) == view ? nullptr : (other[pos++] = std::get<pool_type<Component> *>(pools))), ...);
        return other;
    }

    template<typename Comp, typename Other>
    inline Other & get([[maybe_unused]] component_iterator_type<Comp> it, [[maybe_unused]] const Entity entity) const ENTT_NOEXCEPT {
        if constexpr(std::is_same_v<Comp, Other>) {
            return *it;
        } else {
            return std::get<pool_type<Other> *>(pools)->get(entity);
        }
    }

    template<typename Comp, typename Func, std::size_t... Indexes>
    void each(pool_type<Comp> *cpool, Func func, std::index_sequence<Indexes...>) const {
        const auto other = unchecked(cpool);
        std::array<underlying_iterator_type, sizeof...(Indexes)> data{{std::get<Indexes>(other)->begin()...}};
        const auto extent = std::min({ std::get<pool_type<Component> *>(pools)->extent()... });
        auto raw = std::make_tuple(std::get<pool_type<Component> *>(pools)->begin()...);
        const auto end = cpool->sparse_set<Entity>::end();
        auto begin = cpool->sparse_set<Entity>::begin();

        // we can directly use the raw iterators if pools are ordered
        if constexpr(std::is_invocable_v<Func, std::add_lvalue_reference_t<Component>...>) {
            for(; ((begin != end) && ... && (*begin == *(std::get<Indexes>(data)++))); ++begin) {
                func(*(std::get<component_iterator_type<Component>>(raw)++)...);
            }
        } else {
            while(((begin != end) && ... && (*begin == *(std::get<Indexes>(data)++)))) {
                func(*(begin++), *(std::get<component_iterator_type<Component>>(raw)++)...);
            }
        }

        // fallback to visit what remains using indirections
        while(begin != end) {
            const auto entity = *(begin++);
            const auto it = std::get<component_iterator_type<Comp>>(raw)++;
            const auto sz = size_type(entity & traits_type::entity_mask);

            if(((sz < extent) && ... && std::get<Indexes>(other)->fast(entity))) {
                // avoided at least the indirection due to the sparse set for the pivot type (see get for more details)
                if constexpr(std::is_invocable_v<Func, std::add_lvalue_reference_t<Component>...>) {
                    func(get<Comp, Component>(it, entity)...);
                } else {
                    func(entity, get<Comp, Component>(it, entity)...);
                }
            }
        }
    }

public:
    /*! @brief Underlying entity identifier. */
    using entity_type = typename sparse_set<Entity>::entity_type;
    /*! @brief Unsigned integer type. */
    using size_type = typename sparse_set<entity_type>::size_type;
    /*! @brief Input iterator type. */
    using iterator_type = iterator;

    /*! @brief Default copy constructor. */
    query(const query &) = default;
    /*! @brief Default move constructor. */
    query(query &&) = default;

    /*! @brief Default copy assignment operator. @return This query. */
    query & operator=(const query &) = default;
    /*! @brief Default move assignment operator. @return This query. */
    query & operator=(query &&) = default;

    /**
     * @brief Estimates the number of entities that have the given components.
     * @return Estimated number of entities that have the given components.
     */
    size_type size() const ENTT_NOEXCEPT {
        return std::min({ std::get<pool_type<Component> *>(pools)->size()... });
    }

    /**
     * @brief Checks if a query is definitely empty.
     * @return True if the query is definitely empty, false otherwise.
     */
    bool empty() const ENTT_NOEXCEPT {
        return (std::get<pool_type<Component> *>(pools)->empty() || ...);
    }

    /**
     * @brief Returns an iterator to the first entity that has the given
     * components.
     *
     * The returned iterator points to the first entity that has the given
     * components. If the query is empty, the returned iterator will be equal to
     * `end()`.
     *
     * @note
     * Input iterators stay true to the order imposed to the underlying data
     * structures.
     *
     * @return An iterator to the first entity that has the given components.
     */
    iterator_type begin() const ENTT_NOEXCEPT {
        const auto *view = candidate();
        return iterator_type{unchecked(view), view->begin(), view->end()};
    }

    /**
     * @brief Returns an iterator that is past the last entity that has the
     * given components.
     *
     * The returned iterator points to the entity following the last entity that
     * has the given components. Attempting to dereference the returned iterator
     * results in undefined behavior.
     *
     * @note
     * Input iterators stay true to the order imposed to the underlying data
     * structures.
     *
     * @return An iterator to the entity following the last entity that has the
     * given components.
     */
    iterator_type end() const ENTT_NOEXCEPT {
        const auto *view = candidate();
        return iterator_type{unchecked(view), view->end(), view->end()};
    }

    /**
     * @brief Finds an entity.
     * @param entity A valid entity identifier.
     * @return An iterator to the given entity if it's found, past the end
     * iterator otherwise.
     */
    iterator_type find(const entity_type entity) const ENTT_NOEXCEPT {
        const auto *view = candidate();
        const auto last = end();
        const iterator_type it{unchecked(view), view->find(entity), view->end()};
        return (it != last && *it == entity) ? it : last;
    }

    /**
     * @brief Checks if a query contains an entity.
     * @param entity A valid entity identifier.
     * @return True if the query contains the given entity, false otherwise.
     */
    bool contains(const entity_type entity) const ENTT_NOEXCEPT {
        return !(find(entity) == end());
    }

    /**
     * @brief Returns the components assigned to the given entity.
     *
     * Prefer this function instead of `registry::get` during iterations. It has
     * far better performance than its companion function.
     *
     * @warning
     * Attempting to use an invalid component type results in a compilation
     * error. Attempting to use an entity that doesn't belong to the query
     * results in undefined behavior.<br/>
     * An assertion will abort the execution at runtime in debug mode if the
     * query doesn't contain the given entity.
     *
     * @tparam Comp Types of components to get.
     * @param entity A valid entity identifier.
     * @return The components assigned to the entity.
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
     * @brief Iterates entities and components and applies the given function
     * object to them.
     *
     * The function object is invoked for each entity. It is provided with the
     * entity itself and a set of references to all its components. The
     * _constness_ of the components is as requested.<br/>
     * The signature of the function must be equivalent to one of the following
     * forms:
     *
     * @code{.cpp}
     * void(const entity_type, Component &...);
     * void(Component &...);
     * @endcode
     *
     * @tparam Func Type of the function object to invoke.
     * @param func A valid function object.
     */
    template<typename Func>
    void each(Func func) const {
        const auto *view = candidate();
        ((std::get<pool_type<Component> *>(pools) == view ? each<Component>(std::get<pool_type<Component> *>(pools), std::move(func), std::make_index_sequence<sizeof...(Component)-1>{}) : void()), ...);
    }

private:
    std::tuple<pool_type<Component> *...> pools;
};


}


#endif // ENTT_ENTITY_QUERY_HPP
