##########################################################################
###
### Create Intraguild Social Networks for Each Guild
###
##########################################################################

### Create the social graph for a given prediction date based on the observed data
create_intraguild_graphs <- function(data, pred_date, time_window){

    library(dplyr)
    library(data.table)

    data <- as.data.table(data)

    pred_date <- as.Date(pred_date)
    # keep only the events before the current prediction date
    #data <- data %>% filter(current_date < pred_date)
    data <- data[current_date < pred_date]

    current_guilds <- data[, tail(.SD, 1), by = avatar, .SDcols = c("guild")][guild != -1,]
    # current_guilds <- data %>%
    #     group_by(avatar) %>%
    #     dplyr::slice(n()) %>%
    #     group_by() %>%
    #     filter(guild != -1) %>%
    #     select(avatar, guild)


    # filter out records from horde cities (as only horde avatars are in the dataset)
    # data_with_cities <- data
    horde_capitals <- c("Shattrath City", "Orgrimmar", "Silvermoon City", "Thunder Bluff", "Undercity", "Dalaran")
    data <- data[!(zone %in% horde_capitals)]
    #data <- data %>% filter(!(zone %in% horde_capitals))


    ##### NODES
    print("Getting nodes")

    # Create nodes of G_intra graphs
    # G_intra: contains only current guild members
    nodes <- current_guilds

    # Create nodes of G_intra' graphs
    # G_intra': contains both current and former guild members
    nodes_dash <- unique(data[guild != -1, .(avatar, guild)])
    # nodes_dash <- data %>%
    #     filter(guild != -1) %>%
    #     select(avatar, guild) %>%
    #     distinct(avatar, guild, .keep_all = TRUE)

    nodes_former <- unique(rbind(nodes, nodes_dash)[, .(n = .N), by = .(avatar, guild)][n < 2, .(avatar, guild)])
    # nodes_former <- rbind(nodes, nodes_dash) %>%
    #     group_by(avatar, guild) %>%
    #     mutate(n = n()) %>%
    #     group_by() %>%
    #     filter(n < 2) %>%
    #     mutate(n = NULL) %>%
    #     distinct(avatar, guild, .keep_all = TRUE)


    ##### EDGES
    # The edges of every intraguild social network can be computed at the same time, as collaboration time does not depend on guild membership (two avatars can collaborate even if they are not in the same guild)
    # When we are going to look for the edges of the network of a certain guild, then we only have to perform a simple filtering
    print("Create weighted edges based on cooccurence snapshots")

    ### Function for computing the weights of edges
    # This function only considers collaborations in the last M days, and simply counts the cooccurence snapshots
    compute_weight_component <- function(interaction_dates, cooccurence_snapshot_count = 1, M = time_window){
        sum(cooccurence_snapshot_count[
            as.numeric(difftime(pred_date, interaction_dates, units = "days")) <= M
            ])
    }

    print("Getting all edges")
    interactions <- as.data.table(interactions)
    edges <- interactions[
            current_date < pred_date & difftime(pred_date, current_date, units = "days") <= time_window,
            .(current_weight = compute_weight_component(current_date, collaboration)),
            by = .(avatar.x, avatar.y)
        ][ current_weight > 0, .(node_1 = avatar.x, node_2 = avatar.y, weight = current_weight)]
    # tmp <- interactions %>%
    #  filter(current_date < pred_date & difftime(pred_date, current_date, units = "days") <= time_window) %>%
    #     group_by(avatar.x, avatar.y) %>%
    #     summarise(current_weight = compute_weight_component(current_date, collaboration)) %>%
    #     group_by %>%
    #     filter(current_weight > 0)
    # edges <- data.frame(node_1 = as.character(tmp$avatar.x), node_2 = as.character(tmp$avatar.y), weight = tmp$current_weight)
    # edges <- edges %>% mutate(node_1 = as.character(node_1), node_2 = as.character(node_2))

    print("Getting edges of graph_intra")
    edges_with_current_guilds <- edges
    # edges_with_current_guilds <- edges

    edges_with_current_guilds <-
        current_guilds[edges_with_current_guilds[, .(node_1, node_2, weight, avatar = node_1)],
                         .(node_1, node_2, weight, current_guild_node_1 = guild, avatar = node_2),
                         on = "avatar"]
    edges_with_current_guilds <-
        current_guilds[edges_with_current_guilds,
                         .(node_1, node_2, weight, current_guild_node_1, current_guild_node_2 = guild),
                         on = "avatar"][!is.na(current_guild_node_1) & !is.na(current_guild_node_2)]
    # edges_with_current_guilds$avatar <- as.character(edges_with_current_guilds$node_1)
    # edges_with_current_guilds <- left_join(edges_with_current_guilds, current_guilds, by = "avatar")
    # edges_with_current_guilds$current_guild_node_1 <- edges_with_current_guilds$guild
    # edges_with_current_guilds$guild <- NULL
    # edges_with_current_guilds$avatar <- as.character(edges_with_current_guilds$node_2)
    # edges_with_current_guilds <- left_join(edges_with_current_guilds, current_guilds, by = "avatar")
    # edges_with_current_guilds$current_guild_node_2 <- edges_with_current_guilds$guild
    # edges_with_current_guilds$avatar <- NULL
    # edges_with_current_guilds$guild <- NULL
    # edges_with_current_guilds <- edges_with_current_guilds %>%
    #     filter(!is.na(current_guild_node_1) & !is.na(current_guild_node_2))

    # edges within guilds between current members
    edges_intra <- edges_with_current_guilds[
        current_guild_node_1 == current_guild_node_2 & current_guild_node_1 != -1,
        .(node_1, node_2, weight, guild = current_guild_node_1)
    ]
    # edges_intra <- edges_with_current_guilds %>%
    #     filter(current_guild_node_1 == current_guild_node_2 & current_guild_node_1 != -1) %>%
    #     mutate(guild = current_guild_node_1) %>%
    #     select(node_1, node_2, weight, guild)

    # edges within guilds between both current and former members
    print("Getting edges of graph_intra_dash")

    tmp <- merge(edges_with_current_guilds[, .(node_1, node_2, weight, avatar = node_1)],
                 nodes_dash, all.x = TRUE, by = "avatar",
                 allow.cartesian=TRUE)
    edges_intra_dash <- merge(tmp[, .(node_1, node_2, weight, avatar = node_2, guild)],
                  nodes_dash,
                  by = c("avatar", "guild"),
                  allow.cartesian=TRUE)[, -"avatar", with = FALSE]

    # edges_intra_dash <- nodes_dash %>%
    #     group_by(guild) %>%
    #     do({
    #         current_df <- .
    #         tmp <- edges_with_current_guilds %>%
    #             filter(node_1 %in% current_df$avatar & node_2 %in% current_df$avatar) %>%
    #             mutate(guild = current_df$guild[1]) %>%
    #             select(node_1, node_2, weight, guild)
    #         tmp
    #     }) %>%
    #     group_by()

    # edges within guilds between members of which at least one is a former member
    # ~ (edges_intra_dash - edges_intra)

    print("Getting auxiliary variable edges_intra_former")
    edges_intra_former <-
        (rbind(edges_intra, edges_intra_dash))[
            ,
            .(n = .N),
            by = .(node_1, node_2, guild)
        ][(rbind(edges_intra, edges_intra_dash)), on = c("node_1", "node_2", "guild")
        ][n < 2, -"n", with = FALSE]
    setkey(edges_intra_former, node_1, node_2, guild)
    edges_intra_former <- unique(edges_intra_former)

    # edges_intra_former <- rbind(edges_intra, edges_intra_dash) %>%
    #     group_by(node_1, node_2, guild) %>%
    #     mutate(n = n()) %>%
    #     group_by() %>%
    #     filter(n < 2) %>%
    #     mutate(n = NULL) %>%
    #     distinct(node_1, node_2, guild, .keep_all = TRUE)

    ##### RETURN
    ### Notations:
    # nodes: avatars with their current guild (no-guild records (where guild == -1) are excluded)
    # nodes_dash: avatars with their current and former guilds (i.e. one avatar can have multiple rows/nodes) (no-guild records (where guild == -1) are excluded)
    # nodes_former: nodes_dash - nodes
    # edges: collaborations between avatars
    # edges_intra: edges within guilds, between current members
    # edges_intra_dash: edges within guilds between both current and former members
    # edges_intra_former: edges_intra_dash - edges_intra
    print("create_intraguild_graphs --- return")
    list(nodes = nodes,
         nodes_dash = nodes_dash,
         nodes_former = nodes_former,
         edges = edges,
         edges_intra = edges_intra,
         edges_intra_dash = edges_intra_dash,
         edges_intra_former = edges_intra_former)
}

