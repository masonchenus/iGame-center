#!/bin/bash
mkdir -p backups
cp graphql/data.js backups/data_$(date +%F_%H-%M-%S).js
echo "✅ Backup created!"
