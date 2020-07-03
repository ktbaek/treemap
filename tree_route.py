from math import sqrt, cos, asin, isnan
import numpy as np
import random
import pandas as pd
import folium
import copy
import itertools


class Tree:
    def __init__(self, id, sp, name, year, lon, lat, district):
        self.id = id
        self.name = name
        self.year = year
        self.sp = sp
        self.lon = lon
        self.lat = lat
        self.district = district


#function to calculate distance in km between two geographical coordinates
def distance(t1, t2):
    p = 0.017453292519943295
    a = 0.5 - cos((t2.lat - t1.lat) * p)/2 + cos(t1.lat * p) * cos(t2.lat * p) * (1 - cos((t2.lon - t1.lon) * p)) / 2
    return 12742 * asin(sqrt(a))


def nearest(trees, query):
    d = [(tree, distance(query, tree)) for tree in trees if tree != query]
    return [pair[0] for pair in sorted(d, key = lambda x: x[1])]


#function to insert trees of one species into an already existing graph
def insert_species_into(trees, graph, age_reward_factor = 0.02, set_na_to = 1980):

    #Test if input is ok
    included_species = [tree.sp for tree in graph.keys()]

    if any([tree.sp in included_species for tree in trees]):
        print("species already present")
        print(set([tree.sp for tree in trees]))
        return graph

    if len(set([tree.sp for tree in trees])) != 1:
        print("input contains different species")
        return graph

    #Loop through the input trees and for each tree find the nearest tree already in the graph.
    #Then calculate the cost of inserting the new tree between that tree and each
    #of its two neighbors. The cost is the increased travel distance minus an age reward. The older the tree, the
    #bigger the age reward. The weight of the age reward can be set with age_reward_factor. Age-reward_factor == 0
    #means that age is disregarded.

    g = copy.deepcopy(graph)

    cost_list = []
    for tree in trees:
        year = [tree.year, set_na_to][isnan(tree.year)] #if year is nan, it's set to set_na_to
        age_reward = age_reward_factor * (2020 - year)
        age_reward = [age_reward, 0][isnan(age_reward)]
        nearest_tree = nearest([tree for tree in g.keys()] , tree)[0]
        for neighbor in g[nearest_tree]:
            dist_before = distance(nearest_tree, neighbor)
            dist_after = distance(nearest_tree, tree) + distance(tree, neighbor)
            cost = dist_after - dist_before - age_reward
            cost_list.append([tree, nearest_tree, neighbor, cost])

    #choose the tree with the least cost and insert that into the graph
    tree_to_insert, new_neighbor_1, new_neighbor_2, x = min(cost_list, key = lambda x: x[3])

    g[tree_to_insert] = [new_neighbor_1, new_neighbor_2]
    g[new_neighbor_1].remove(new_neighbor_2)
    g[new_neighbor_2].remove(new_neighbor_1)
    g[new_neighbor_1].append(tree_to_insert)
    g[new_neighbor_2].append(tree_to_insert)

    return g


def tour_length(tour):
    dist = 0
    for i in range(len(tour)-1):
        dist += distance(tour[i], tour[i+1])
    dist += distance(tour[-1], tour[0])

    return dist


def list_to_graph(tour):

    g = {tree: [tour[i+1]] for i, tree in enumerate(tour[:-1])}
    g[tour[-1]] = [tour[0]]

    temp = dict((v[0],k) for k,v in g.items())

    for k, v in temp.items():
        g[k].append(v)

    return g


def graph_to_list(graph):
    tour = [random.choice(list(graph.keys()))]
    while len(tour) < len(graph):
        if graph[tour[-1]][0] not in tour:
            tour.append(graph[tour[-1]][0])
        else:
            tour.append(graph[tour[-1]][1])
    return tour


#function to generate a route between trees based on nearest-neighbor
def nearest_neighbor(trees):

    current_tree = trees[0]
    g = {current_tree: []}

    while set(g.keys()) != set(trees):
        unvisited = [tree for tree in trees if tree not in g.keys()]
        next_tree = nearest(unvisited, current_tree)[0]
        g[current_tree].append(next_tree)
        g[next_tree] = [current_tree]
        current_tree = next_tree

    g[trees[0]].append(current_tree)
    g[current_tree].append(trees[0])

    return g


#funtion to optimize an existing route
def two_opt(graph):

    current_tour = graph_to_list(graph)

    k = 0

    for i in range(len(current_tour))[:-3]:
        for j in range(len(current_tour))[i+2:-1]:
            old_edge_1 = distance(current_tour[i],current_tour[i+1])
            old_edge_2 = distance(current_tour[j],current_tour[j+1])
            new_edge_1 = distance(current_tour[i],current_tour[j])
            new_edge_2 = distance(current_tour[i+1],current_tour[j+1])

            swap_improvement = old_edge_1 + old_edge_2 > new_edge_1 + new_edge_2

            if(swap_improvement):
                current_tour[i+1:j+1] = current_tour[i+1:j+1][::-1]
                k += 1

    #the edge between tour[-1] and tour[0]
    for j in range(len(current_tour))[1:-2]:
        old_edge_1 = distance(current_tour[-1],current_tour[0])
        old_edge_2 = distance(current_tour[j],current_tour[j+1])
        new_edge_1 = distance(current_tour[-1],current_tour[j])
        new_edge_2 = distance(current_tour[0],current_tour[j+1])

        swap_improvement = old_edge_1 + old_edge_2 > new_edge_1 + new_edge_2

        if(swap_improvement):
            current_tour[j+1:] = current_tour[j+1:][::-1]
            k += 1

    print('swaps: ' + str(k))
    print('tour length before 2-opt: ' + str(tour_length(graph_to_list(graph))))
    print('tour length after 2-opt: ' + str(tour_length(current_tour)))

    return list_to_graph(current_tour)


def repeat_two_opt(tour, times):

    new_tour = tour.copy()

    for _ in range(times):
        new_tour = two_opt(new_tour)

    return new_tour


def show_tour(graph):
    m = folium.Map(location=[55.68, 12.6], tiles = 'CartoDB Positron', zoom_start = 12)
    for tree, neighbors in graph.items():
        folium.CircleMarker([tree.lat,tree.lon], popup = tree.id).add_to(m)
        for n in neighbors:
            folium.PolyLine([[tree.lat,tree.lon], [n.lat,n.lon]], color='blue').add_to(m)
    return m


#function that runs nearest-neighbor followed by repeated 2-optimizations
def make_initial_route(trees, opt_repeats = 4):
    species = set([tree.sp for tree in trees])

    single_trees = []
    for sp in species:
        trees_sp = [tree for tree in trees if tree.sp == sp]
        if len(trees_sp) == 1:
            single_trees = single_trees + trees_sp

    t = nearest_neighbor(single_trees)
    t = repeat_two_opt(t, opt_repeats)

    return t


#function that insert new trees into existing route, species by species
def insert_trees(initial_route, trees, opt_repeats = 4, age_reward_factor = 0, set_na_to = 1980):

    t = initial_route.copy()

    species = set([tree.sp for tree in trees])

    for sp in species:
        trees_sp = [tree for tree in trees if tree.sp == sp]
        if len(trees_sp) > 1:
            t = insert_species_into(trees_sp, t, age_reward_factor, set_na_to)

    print('trees: ' + str(len(t)))
    print('avg year: ' + str(np.nanmean([tree.year for tree in graph_to_list(t)])))

    t = repeat_two_opt(t,5)

    return t


# #### Import trees

#all_trees.csv is prepared in R after clean-up of original data from Københavns Kommune

all_trees_df = pd.read_csv("all_trees.csv")
all_trees_df["planteaar"] = all_trees_df["planteaar"].astype('Int64')

all_trees = []

for index, row in all_trees_df.iterrows():
    all_trees.append(Tree(id = row["id"],
                             name = row["dansk_navn"],
                             sp = row["art"],
                             year = row["planteaar"],
                             lon = row["lon"],
                             lat = row["lat"],
                             district = row["bydelsnavn"])
                    )

all_trees = tuple(all_trees)


# #### Make path among trees of which there's only one tree per species

initial_route = make_initial_route(all_trees)
show_tour(initial_route)


# #### Insert remaining trees into initial route

whole_route = insert_trees(initial_route, all_trees, age_reward_factor = 5)
show_tour(whole_route)


route_df = pd.DataFrame([tree.id for tree in whole_route],
                        columns =['id'])
