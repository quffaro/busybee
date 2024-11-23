<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Elegant Red Template</title>
    <style>
        :root {
            --primary-red: #8B0000;
            --light-red: #FFE4E4;
            --dark-red: #4A0000;
        }

        body {
            font-family: 'Charter', serif;
            line-height: 1.6;
            margin: 0;
            padding: 0;
            color: #333;
            background-color: #fff;
        }

        header {
            background-color: var(--primary-red);
            color: white;
            padding: 2rem;
            text-align: center;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        nav {
            background-color: var(--dark-red);
            padding: 1rem;
            text-align: center;
        }

        nav a {
            color: white;
            text-decoration: none;
            padding: 0.5rem 1rem;
            margin: 0 0.5rem;
            transition: background-color 0.3s;
        }

        nav a:hover {
            background-color: rgba(255,255,255,0.1);
            border-radius: 4px;
        }

        main {
            max-width: 800px;
            margin: 2rem auto;
            padding: 0 1rem;
        }

        .card {
            background-color: white;
            border: 1px solid #ddd;
            border-radius: 8px;
            padding: 1.5rem;
            margin-bottom: 2rem;
            box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        }

        h1, h2, h3 {
            color: var(--primary-red);
        }

        footer {
            background-color: var(--light-red);
            color: var(--dark-red);
            text-align: center;
            padding: 1rem;
            position: fixed;
            bottom: 0;
            width: 100%;
        }

        .btn {
            background-color: var(--primary-red);
            color: white;
            padding: 0.5rem 1rem;
            border: none;
            border-radius: 4px;
            cursor: pointer;
            transition: background-color 0.3s;
        }

        .btn:hover {
            background-color: var(--dark-red);
        }
    </style>
    <script type="text/x-mathjax-config">
        MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
    </script>
    <script type="text/javascript"
        src="http://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
    </script>
</head>
<body>
    <header>
        <h1>Welcome to Elegant Red</h1>
        <p>A sophisticated and minimalist design</p>
    </header>

    <nav>
        <a href="#home">Home</a>
        <a href="#about">About</a>
        <a href="#services">Services</a>
        <a href="#contact">Contact</a>
    </nav>

    <main>
        
        <div class="card">
            <h2>Body</h2>
            <body>◊(->html doc)</body>
        </div>
    </main>

    <footer>
        <p>&copy; 2024 Elegant Red Template. All rights reserved.</p>
    </footer>
</body>
</html>
