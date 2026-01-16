library(shiny)
# ==== styleCSS.R ====
styleCSS <- tags$head(
  # Import FontAwesome untuk ikon
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
  ),
  
  tags$style(HTML("
.container-fluid {
    padding-left: 5px !important;
    padding-right: 5px !important;
  }
  /* ================================
     STYLE UNTUK DATATABLE
  ================================ */
  table.dataTable {
    font-size: 12px !important;
    line-height: 0.7 !important;
    letter-spacing: -0.3px !important;
    border-collapse: collapse !important;
  }

  table.dataTable th,
  table.dataTable td {
    padding: 4px 6px !important;
  }

  table.dataTable thead th {
    background-color: darkcyan !important;
    color: white !important;
    height: 0.7 !important;
    font-weight: bold !important;
  }

  table.dataTable tbody tr:nth-child(even) {
    background-color: #f2f6fa !important;
  }

  /* ================================
     STYLE UNTUK TABEL BIASA
  ================================ */
  table, th, td {
    font-size: 12px !important;
    line-height: 0.7 !important;
    letter-spacing: -0.3px !important;
    border-collapse: collapse !important;
  }

  th {
    background-color: darkcyan !important;
    color: white !important;
    font-weight: bold !important;
  }

  tr:nth-child(even) {
    background-color: #f2f6fa !important;
  }

  td, th {
    padding: 4px 6px !important;
  }

  /* ================================
     STYLE UNTUK TOMBOL (BUTTON)
  ================================ */
  .btn {
    width: auto !important;
    padding: 5px 10px !important;
    font-size: 12px !important;
    line-height: 1 !important;
    border-radius: 8px !important;
    font-weight: 700 !important;
    transition: all 0.1s ease-in-out !important;
    display: block;
    margin: 0 auto;
  }

  .btn i {
    margin-right: 6px !important;
  }

  .btn:hover {
    box-shadow: 0 2px 6px rgba(0,0,0,0.2) !important;
  }

  .btn:active {
    transform: scale(0.97) !important;
    box-shadow: 0 1px 3px rgba(0,0,0,0.3) !important;
  }

  /* Warna Tombol Khusus */
  .btn-primary {
    background-color: #0077dd !important;
    border: none !important;
  }
  .btn-primary:hover {
    background-color: #0056b3 !important;
  }

  .btn-danger {
    background-color: #fd4084 !important;
    border: none !important;
  }
  .btn-danger:hover {
    background-color: #a71d2a !important;
  }

  .btn-success {
    background-color: #28a745 !important;
    border: none !important;
  }
  .btn-success:hover {
    background-color: #1e7e34 !important;
  }
  /* =======================================
     STYLE   DT BUTTONS
   ======================================= */

.dt-button {
  padding: 4px 10px !important;    
  font-size: 9px !important;        
  border-radius: 6px !important; 
  background-color: darkcyan !important;
  color: white !important;
  border: none !important;
  font-weight: 600 !important;
  margin: 3px !important;
  line-height: 1 !important;
}

/* Hover */
.dt-button:hover {
  background-color: #005bb0 !important;
}

/* Active */
.dt-button:active {
  transform: scale(0.97) !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
    font-size: 10px !important; 
    padding: 2px 6px !important;     
    border-radius: 6px !important;  
  }

  .dataTables_wrapper .dataTables_paginate {
    margin-top: 2px !important;      
  }

  /* ================================
     STYLE UNTUK INPUT (select, numeric, file)
  ================================ */
  select.form-control,
  input.form-control,
  .selectize-input,
  .selectize-dropdown-content {
    border-radius: 12px !important;
    font-size: 12px !important;
    line-height: 1.2 !important;
    border: 1px solid #ddd !important;
  }

  .selectize-input {
    padding: 6px 10px !important;
    min-height: 32px !important;
  }

  .selectize-dropdown-content {
    font-size: 12px !important;
  }

  /* ================================
     STYLE UNTUK HOMEPAGE / LANDING PAGE
  ================================ */
  body {
    background: #fff;
    font-family: 'Poppins', sans-serif;
    margin: 0;
    padding: 0;
  }

  .landing {
    text-align: center;
    margin-top: 25px;
  }

  .hero {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 5px;
  }

  .welcome {
    font-size: 16px;
    font-weight: 800;
    color: #444;
    letter-spacing: 1px;
  }

.app-name { font-size:36px; font-weight:900; margin-top:1px; color:transparent; -webkit-text-fill-color:black; -webkit-background-clip:text; background:none; }
.app-name { text-shadow:-2px -2px 3px #fff,2px -2px 3px #fff,-2px 2px 3px #fff,2px 2px 3px #fff,0 0 5px rgba(0,0,0,.45),0 0 10px rgba(0,0,0,.35),0 0 15px rgba(0,0,0,.25); }

  .subtitle{
    font-size: 13px;
    font-weight: 600;
    color: #444;
  }

  h4.page-subtitle {
    margin-top: 20px;
    color: #333;
  }

  .quad-container {
    position: relative;
    z-index: 0;
    width: 90vw;
    height: 60vh;
    margin: 10px auto;
    display: grid;
    grid-template-columns: 1fr 1fr;
    grid-template-rows: 1fr 1fr;
    gap: 10px;
    justify-items: center;
    align-items: center;
  }

  .quad-container::before,
  .quad-container::after {
    content: '';
    position: absolute;
    background: blue;
    z-index: 1;
  }

  .quad-container::before {
    top: 50%;
    left: 0;
    width: 100%;
    height: 2px;
  }

  .quad-container::after {
    left: 50%;
    top: 0;
    width: 2px;
    height: 100%;
  }

  .axis-label {
    position: absolute;
    font-weight: bold;
    color: blue;
    font-size: clamp(11px, 2vw, 16px);
    background: white;
    border-radius: 6px;
    z-index: 5;
  }

  .axis-label.x-top {
    top: 0;
    left: 50%;
    transform: translateX(-50%);
  }

  .axis-label.x-bottom {
    bottom: 0;
    left: 50%;
    transform: translateX(-50%);
  }

  .axis-label.y-left {
    left: 0;
    top: 50%;
    transform: translateY(-50%);
  }

  .axis-label.y-right {
    right: 0;
    top: 50%;
    transform: translateY(-50%);
  }

  .center-logo {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: 6;
    padding: 6px;
  }

  .center-logo img {
    width: clamp(70px, 2vw, 200px);
    height: auto;
    border-radius: 50%;
  }

  .quad-card {
    width: 90%;
    height: 80%;
    border-radius: 16px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.15);
    padding: 1.5vw;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    align-items: center;
    text-align: center;
    z-index: 2;
    transition: transform .2s;
  }

  .quad-card:hover {
    transform: scale(1.05);
  }
#card_lca { background: #eef3ff; border: 2px solid #c7d4ff; }   /* Blue */
#card_lpa { background: #edf7f6; border: 2px solid #c6e6e2; }   /* Teal */
#card_fa  { background: #f3effa; border: 2px solid #d9cff0; }   /* Purple */
#card_lta { background: #fbeef2; border: 2px solid #f2cdd8; }   /* Rose */
/* Warna & tema masing-masing card */
#card_lpa .project-icon { background: #dff1ee; color: #138f87; } /* Teal */
#card_lca .project-icon { background: #dde6ff; color: #2a55e6; } /* Blue */
#card_fa  .project-icon { background: #e6e0f5; color: #6b4cc2; } /* Purple */
#card_lta .project-icon { background: #f6dde6; color: #c23b63; } /* Rose */

#card_lca .btn-pill { background: #2a55e6; }  /* Blue */
#card_lpa .btn-pill { background: #138f87; }  /* Teal */
#card_fa  .btn-pill { background: #6b4cc2; }  /* Purple */
#card_lta .btn-pill { background: #c23b63; }  /* Rose */

  .project-title {
    font-size: clamp(12px, 1.5vw, 27px);
    font-weight: 600;
    margin-bottom: 4px;
  }

  .project-desc {
    color: #444;
    font-size: clamp(12px, 1vw, 120px);
    line-height: 1.3em;
    padding: 0 .5vw;
  }

  .project-icon {
    font-size: clamp(25px, 2vw, 40px);
    width: clamp(40px, 4vw, 60px);
    height: clamp(40px, 4vw, 60px);
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 12px;
  }



  .btn-pill {
    display: block !important;
    width: 100% !important;
    padding: clamp(5px, .8vw, 10px) 0;
    border-radius: 8px;
    text-align: center;
    text-decoration: none;
    margin-top: 3px;
    color: #fff;
    font-size: clamp(9px, 1vw, 18px);
    transition: .1s;
  }

  .btn-pill:hover {
    opacity: .85;
    transform: scale(1.001);
  }
  
   /* Styling untuk labels */
         .shiny-input-container label {
           font-size: 12px !important;
           font-weight: normal !important;
           color: #555555 !important;
           margin-bottom: 3px !important;
         }
         
         /* Styling untuk input fields */
         .form-control {
           font-size: 12px !important;
           height: 30px !important;
           padding: 4px 8px !important;
         }
         
         /* Styling khusus untuk numeric input */
         input[type='number'] {
           font-size: 12px !important;
           height: 28px !important;

         }
         
         /* Styling untuk select input */
         select.form-control {
           font-size: 12px !important;
           height: 30px !important;
           padding: 4px 8px !important;
         }
/* === Style khusus untuk selectInput besar (itemtype) === */
.select-large .selectize-input {
  font-size: 18px !important;
  font-weight: bold !important;
  text-align: center !important;
  padding: 12px 16px !important;
  border-radius: 14px !important;
  background-color: lightgreen !important;
  border: 2px solid #cfcfcf !important;
  box-shadow: 0 3px 6px rgba(0,0,0,0.1) !important;
  transition: all 0.2s ease-in-out !important;
}

.select-large .selectize-input:hover {
  border-color: #007bff !important;
  box-shadow: 0 0 8px rgba(0,123,255,0.3) !important;
}

.select-large .selectize-input.focus {
  border-color: #0056b3 !important;
  box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25) !important;
}

.select-large .option {
  text-align: center !important;
  font-size: 16px !important;
  font-weight: bold !important;
}

/* === Style selectInput mini tanpa highlight pilihan (versi tinggi lebih pendek) === */
.select-mini .selectize-input {
  font-size: 12px !important;
  font-weight: 600 !important;
  text-align: center !important;
  justify-content: center !important;

  padding: 1px 6px !important;          
  line-height: 12px !important;  
  min-height: 18px !important;     
  height: 18px !important;
  
  border-radius: 8px !important;
  background: lightgreen !important;
  border: 1.5px solid #b8bced !important;
  box-shadow: 0 2px 3px rgba(0,0,0,0.07) !important;
  transition: all 0.2s ease-in-out !important;
}

.select-mini .selectize-input:hover {
  border-color: #756bff !important;
  box-shadow: 0 0 5px rgba(117,107,255,0.33) !important;
}

.select-mini .selectize-input.focus {
  border-color: #574cff !important;
  box-shadow: 0 0 0 0.15rem rgba(87,76,255,0.22) !important;
}

.select-mini .option {
  text-align: center !important;
  font-size: 12px !important;
  font-weight: 600 !important;
}

/* Hilangkan highlight */
.select-mini .selectize-dropdown .highlight {
  background: none !important;
  color: inherit !important;
  box-shadow: none !important;
}


         /* Styling untuk text input */
         input[type='text'] {
           font-size: 12px !important;
         }
         
         /* Styling untuk checkbox labels */
         .checkbox label {
           font-size: 12px !important;
           font-weight: normal !important;
           color: #555555 !important;
         }
  
  
  .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
      }
      .shiny-output-error { color: #dc3545; }
      .shiny-output-error:before { content: 'Error: '; }
      .small-input { max-width: 100px; }

  @media (max-width: 700px) {
    .quad-container {
      transform: scale(1);
      transform-origin: center top;
    }
  }
  
  "))
)
