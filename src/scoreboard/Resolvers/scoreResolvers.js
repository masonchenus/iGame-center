const { v4: uuidv4 } = require('uuid');
const { scores } = require('../data');

const scoreResolvers = {
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

module.exports = scoreResolvers;
