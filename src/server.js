const { ApolloServer, gql } = require('apollo-server');
const { v4: uuidv4 } = require('uuid');

// In-memory storage for demo purposes
const scores = [];

// GraphQL schema
const typeDefs = gql`
  type Score {
    id: ID!
    player: String!
    value: Int!
  }

  type Query {
    allScores: [Score!]!
  }

  type Mutation {
    addScore(player: String!, value: Int!): Score!
  }
`;

// Resolvers
const resolvers = {
    Query: {
        allScores: () => scores.sort((a, b) => b.value - a.value),
    },
    Mutation: {
        addScore: (_, { player, value }) => {
            const score = { id: uuidv4(), player, value };
            scores.push(score);
            return score;
        },
    },
};

// Create server
const server = new ApolloServer({ typeDefs, resolvers });

server.listen({ port: 4000 }).then(({ url }) => {
    console.log(`🚀 GraphQL server running at ${url}`);
});
