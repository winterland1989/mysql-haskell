#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql/mysql.h>
#include <pthread.h>
#include <unistd.h>

#define THREAD_NUM  4
#define DBHOST      "127.0.0.1"
#define DBUSER      "testMySQLHaskell"
#define DBPASS      ""
#define DBPORT      3306
#define DBNAME      "testMySQLHaskell"
#define DBSOCK      NULL //"/var/lib/mysql/mysql.sock"
#define DBPCNT      0
#define STRING_SIZE 50

typedef struct ThreadArgsST
{
    int id;
    pthread_t *thread_id;
} ThreadArgs;

void *func(void *arg)
{
    ThreadArgs *args = (ThreadArgs *)arg;
    MYSQL_ROW row;
    unsigned int rowCounter = 0;
    MYSQL_FIELD *field;
    unsigned int i;
    unsigned int timeout = 3000;
    const char *pStatement = "SELECT * FROM employees";
    mysql_thread_init();
    MYSQL *mysql = mysql_init(NULL);
    MYSQL_STMT *stmt;
    MYSQL_BIND    bind[6];
    MYSQL_RES     *prepare_meta_result;
    short         small_data;
    int           int_data;
    char          str_data[STRING_SIZE];
    char          str_data2[STRING_SIZE];
    my_bool       is_null[6];
    my_bool       error[6];
    MYSQL_TIME    ts;
    unsigned long length[6];

    if (mysql == NULL)
    {
        printf("[%ld][%d]mysql init failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        return (void *)0;
    }

    mysql_options(mysql, MYSQL_OPT_CONNECT_TIMEOUT, &timeout);

    if (mysql_real_connect(mysql, DBHOST, DBUSER, DBPASS, DBNAME, DBPORT, DBSOCK, DBPCNT) == NULL)
    {
        printf("[%ld][%d]connect failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        mysql_close(mysql);
        mysql_thread_end();
        return (void *)0;
    }

    stmt = mysql_stmt_init(mysql);
    if (0 != mysql_stmt_prepare(stmt, pStatement, 23))
    {
        printf("[%ld][%d]prepare statement failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        mysql_close(mysql);
        mysql_thread_end();
        return (void *)0;
    }

    prepare_meta_result = mysql_stmt_result_metadata(stmt);
    if (!prepare_meta_result)
    {
      printf( "mysql_stmt_result_metadata(), returned no meta information\n");
      printf(" %s\n", mysql_stmt_error(stmt));
      exit(0);
    }

    if (0 != mysql_stmt_execute(stmt))
    {
        printf("[%ld][%d]query failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        mysql_close(mysql);
        mysql_thread_end();
        return (void *)0;
    }

    /* Bind the result buffers for all 4 columns before fetching them */

    memset(bind, 0, sizeof(bind));

    bind[0].buffer_type= MYSQL_TYPE_LONG;
    bind[0].buffer= (char *)&int_data;
    bind[0].is_null= &is_null[0];
    bind[0].length= &length[0];
    bind[0].error= &error[0];

    /* TIMESTAMP COLUMN */
    bind[1].buffer_type= MYSQL_TYPE_DATE;
    bind[1].buffer= (char *)&ts;
    bind[1].is_null= &is_null[1];
    bind[1].length= &length[1];
    bind[1].error= &error[1];

    /* STRING COLUMN */
    bind[2].buffer_type= MYSQL_TYPE_STRING;
    bind[2].buffer= (char *)str_data;
    bind[2].buffer_length= STRING_SIZE;
    bind[2].is_null= &is_null[2];
    bind[2].length= &length[2];
    bind[2].error= &error[2];

    /* STRING COLUMN */
    bind[3].buffer_type= MYSQL_TYPE_STRING;
    bind[3].buffer= (char *)str_data;
    bind[3].buffer_length= STRING_SIZE;
    bind[3].is_null= &is_null[3];
    bind[3].length= &length[3];
    bind[3].error= &error[3];

    /* STRING COLUMN */
    bind[4].buffer_type= MYSQL_TYPE_STRING;
    bind[4].buffer= (char *)str_data;
    bind[4].buffer_length= STRING_SIZE;
    bind[4].is_null= &is_null[4];
    bind[4].length= &length[4];
    bind[4].error= &error[4];

    /* TIMESTAMP COLUMN */
    bind[5].buffer_type= MYSQL_TYPE_DATE;
    bind[5].buffer= (char *)&ts;
    bind[5].is_null= &is_null[5];
    bind[5].length= &length[5];
    bind[5].error= &error[5];

    /* Bind the result buffers */
    if (mysql_stmt_bind_result(stmt, bind))
    {
      printf( " mysql_stmt_bind_result() failed\n");
      printf( " %s\n", mysql_stmt_error(stmt));
      exit(0);
    }
    mysql_stmt_store_result(stmt);

    printf("field name: ");
    while (NULL != (field = mysql_fetch_field(prepare_meta_result)))
    {
        printf(" %s, ", field->name);
    }

    while (!mysql_stmt_fetch(stmt))
    {
        rowCounter++;

    }
    printf("loop through result, total %d rows\n", rowCounter);

    mysql_free_result(prepare_meta_result);
    mysql_close(mysql);
    mysql_thread_end();
    return (void *)0;
}

int main(int argc, char *argv[])
{
    int thread_num;

    if (argc == 2)
    {
        thread_num = atoi(argv[1]);
    }
    else
    {
        thread_num = THREAD_NUM;
    }

    mysql_library_init(0, NULL, NULL);
    printf("argc: %d and thread_num: %d\n", argc, thread_num);

    do
    {
        pthread_t *pTh = new pthread_t[thread_num];
        ThreadArgs *pArgs = new ThreadArgs[thread_num];
        int i;

        for (i = 0; i < thread_num; i ++)
        {
            pArgs[i].id = i;
            pArgs[i].thread_id = &pTh[i];

            if (0 != pthread_create(&pTh[i], NULL, func, &pArgs[i]))
            {
                printf("pthread_create failed\n");
                continue;
            }
        }

        for (i = 0; i < thread_num; i ++)
        {
            pthread_join(pTh[i], NULL);
        }

        delete[] pTh;
        delete[] pArgs;
    }
    while (0);

    mysql_library_end();
    return 0;
}

