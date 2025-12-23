const { v4: uuidv4 } = require('uuid');
const { achievements } = require('../data');

const achievementResolvers = {
    Query: {
        allAchievements: () => achievements,
    },
    Mutation: {
        addAchievement: (_, { player, title, description }) => {
            const achievement = { id: uuidv4(), player, title, description };
            achievements.push(achievement);
            return achievement;
        },
    },
};

module.exports = achievementResolvers;
