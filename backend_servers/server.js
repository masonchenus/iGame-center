import fastify from 'fastify';
import express from 'express';
import cors from 'cors';
import bodyParser from 'body-parser';
import multer from 'multer';
import compression from 'compression';
import helmet from 'helmet';
import morgan from 'morgan';
import passport from 'passport';

const app = fastify();

// Set up CORS
app.register(cors);

// Set up body parsing
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

// Set up file upload
const storage = multer.diskStorage({
    destination: (req, file, cb) => {
        cb(null, 'uploads/');
    },
    filename: (req, file, cb) => {
        cb(null, Date.now() + '-' + file.originalname);
    },
});
const upload = multer({ storage });

// Set up compression
app.use(compression());

// Set up security headers
app.register(helmet());

// Set up request logging
app.use(morgan('combined'));

// Set up authentication
app.use(passport.initialize());

// Define routes
app.get('/', (req, res) => {
    res.send('Hello, World!');
});

// Define other routes

// Start the server
const port = process.env.PORT || 3000;
app.listen(port, (err) => {
    if (err) {
        console.error(err);
        process.exit(1);
    }
    console.log(`Server is running on port ${port}`);
});
