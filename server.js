const { ApolloServer, gql } = require('apollo-server');
const { readFileSync } = require('fs');
const path = require('path');

// Load schema files
const scoreSchema = readFileSync(path.join(__dirname, 'graphql/schema/score.graphql'), 'utf8');
const achievementSchema = readFileSync(path.join(__dirname, 'graphql/schema/achievement.graphql'), 'utf8');
const typeDefs = gql`${scoreSchema}\n${achievementSchema}`;

// Load resolvers
const scoreResolvers = require('./graphql/resolvers/scoreResolvers');
const achievementResolvers = require('./graphql/resolvers/achievementResolvers');

// Merge resolvers
const resolvers = {
    Query: { ...scoreResolvers.Query, ...achievementResolvers.Query },
    Mutation: { ...scoreResolvers.Mutation, ...achievementResolvers.Mutation },
};

// Create server
const server = new ApolloServer({ typeDefs, resolvers });

server.listen({ port: 4000 }).then(({ url }) => {
    console.log(`🚀 GraphQL server running at ${url}`);
});
