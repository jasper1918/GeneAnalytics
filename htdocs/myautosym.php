<?php 
try {
  $conn = new PDO('sqlite:assets/BRCADB_2013_Ann.sqlite');
}
catch(PDOException $e) {
    echo $e->getMessage();
}
 
$return_arr = array();

  if ($conn)
  {
      $symbol = "%".$_GET['term']."%";
      $query ='SELECT DISTINCT gs FROM BRCADBANN WHERE gs LIKE :term ORDER by gs LIMIT 10';
      $stmt = $conn->prepare($query,array(PDO::ATTR_CURSOR => PDO::CURSOR_FWDONLY));
      $stmt->bindValue(":term",$symbol);
      $stmt->execute();
       
      /* Retrieve and store in array the results of the query.*/
      while ($row = $stmt->fetch(PDO::FETCH_ASSOC)) {
	  $row_array['label'] = $row['gs'];
	  $row_array['value'] = $row['gs'];
	   
	  array_push($return_arr,$row_array);
      }
   
       
  }
/* Free connection resources. */
  $conn = null; 
/* Toss back results as json encoded array. */
header("Content-type: application/json"); 
 echo json_encode($return_arr);
?>