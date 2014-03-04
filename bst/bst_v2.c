/**
 * Binary search tree implementation
 *
 * Copyright (c) 2013 the authors listed at the following URL, and/or
 * the authors of referenced articles or incorporated external code:
 * http://en.literateprograms.org/Binary_search_tree_(C)?action=history&offset=20121127201818
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Retrieved from: http://en.literateprograms.org/Binary_search_tree_(C)?oldid=18734
 * Modified: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 */


/***********************************************************/
/* NOTE: You can modify/add any piece of code that will    */
/* make your algorithm work                                */
/***********************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "bst.h"

pthread_spinlock_t lock_cg;
pthread_spinlock_t entry_lock;


/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           the node containg the data
 */
struct bst_node**
search(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: For the Step 2 you will have to make this function thread-safe */

    struct bst_node** node = root;

    while (*node != NULL) {
        int compare_result = compare(data, (*node)->data);

        if (compare_result < 0) {
            node = &(*node)->left;
        } else if (compare_result > 0) {
            node = &(*node)->right;
        } else {
            break;
        }
    }

    return node;
}


/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void
node_delete_aux(struct bst_node** node)
{
    /* TODO: For Step 2 you will have to make this function thread-safe */

    struct bst_node* old_node = *node;

    if ((*node)->left == NULL) {
        *node = (*node)->right; // Node is replaced by its RIGHT node
        free_node(old_node);
    } else if ((*node)->right == NULL) {
        *node = (*node)->left; // Node is replaced by its LEFT node
        free_node(old_node);
    } else {

        // Here we need to lock the node we are looking at AND its parent

        struct bst_node** pred = &(*node)->left; // pred is set to the LEFT node
	
        while ((*pred)->right != NULL) { // Find the most RIGHT node of pred
	    pred = &(*pred)->right;
	}

	/* Swap values */
	void* temp = (*pred)->data; // Set the value of NODE = NODE->LEFT->(most RIGHT node value) 
	(*pred)->data = (*node)->data; 
	(*node)->data = temp;

	node_delete_aux(pred); // Remove NODE->LEFT->(most RIGHT node) instead of NODE
    }
}


/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           1 if data is not found, 0 otherwise
 */
struct bst_node**search_fg(struct bst_node** root, comparator compare, void* data)
{

    if(root != NULL)
        pthread_spin_lock(&(*root)->lock); // lock the current node
   
    struct bst_node** node = root;

    while (*node != NULL) { // Unsafe area
    
        int compare_result = compare(data, (*node)->data);

        if (compare_result < 0) {          
            pthread_spin_unlock(&(*node)->lock); // unlock the current node
            pthread_spin_lock(&((*node)->left)->lock); // lock the next node
            node = &(*node)->left; // Go left
            
        } else if (compare_result > 0) {
            pthread_spin_unlock(&(*node)->lock); // unlock the current node
            pthread_spin_lock(&((*node)->right)->lock); // lock the next node
            node = &(*node)->right; // Go right
        } else {
            break; // Current node is locked
        }

       
    }

    return node; //return locked node
}


/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void node_delete_aux_fg(struct bst_node **node)
{

    struct bst_node* old_node = *node;

    if ((*node)->left == NULL) {

        pthread_spin_lock(&((*node)->right)->lock); //1# lock next node    
        *node = (*node)->right; 
        free(old_node);
        pthread_spin_unlock(&(*node)->lock); //unlock   1#  

    } else if ((*node)->right == NULL) { 

        pthread_spin_lock(&((*node)->left)->lock); //2# lock next node

        *node = (*node)->left;   
        free(old_node);
        pthread_spin_unlock(&(*node)->lock); //2# unlock

    } else {
        
      
        pthread_spin_lock(&((*node)->left)->lock); //#3 lock first node


        struct bst_node** pred = &(*node)->left; 

        while ((*pred)->right != NULL) {
            pthread_spin_lock(&((*pred)->right)->lock); //#4 lock next node
            pthread_spin_unlock(&(*pred)->lock); //#3 lock unlock current node

	    pred = &(*pred)->right; //#3 -> #4

	}
        
        
	/* Swap values */
	void* temp = (*pred)->data; 
	(*pred)->data = (*node)->data; 
	(*node)->data = temp;

      
        pthread_spin_unlock(&(*node)->lock); // unlock current node

	node_delete_aux_fg(pred); //#3
    }


}


/**
 * Deletes the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);

    if (*node == NULL) {
        return -1;
    }

    node_delete_aux(node);

    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_cg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */

    pthread_spin_lock(&lock_cg);

    struct bst_node** node = search(root, compare, data);

    if (node == NULL) {
        pthread_spin_unlock(&lock_cg);
        return -1;
    }
    
    node_delete_aux(node);

    pthread_spin_unlock(&lock_cg);

    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_fg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */
    struct bst_node** node = search_fg(root, compare, data);
    //pthread_spin_unlock(&(*node)->lock);
    if (node == NULL) {
        return -1;
    }
    
    node_delete_aux_fg(node);

    return 0;
}


/**
 * Allocate resources and initialize a BST.
 *
 * @return           root of the BST
 */
struct bst_node **tree_init(void)
{
    struct bst_node** root = malloc(sizeof(*root));
    if (root == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    *root = NULL;

    /* TODO: Initialize any global variables you use for the BST */
    pthread_spin_init(&lock_cg, 0);
    pthread_spin_init(&entry_lock, 0);
    
    return root;
}

/**
 * Remove resources for the tree.
 *
 * @param root       root of the tree
 */
void tree_fini(struct bst_node ** root)
{

    /* TODO: Free any global variables you used for the BST */
    pthread_spin_destroy(&lock_cg);

    if (root != NULL)
        free(root);

}


/**
 * Inserts a new node with the requested data if not already in the tree.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be inserted
 * @return           1 if data is in the BST already, 0 otherwise
 */
int
node_insert(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);
    if (*node == NULL) {
        *node = new_node(data);
        return 0;
    } else
        return 1;
}


/**
 * Creates a new node with the requested data.
 *
 * @param data       pointer to the data pointed be the new node
 */
struct bst_node* 
new_node(void* data)
{
    struct bst_node* node = malloc(sizeof(struct bst_node));
    if (node == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    } else {
        /* TODO: Initialize any per node variables you use for the BST */
        
        node->left = NULL;
        node->right = NULL;
        node->data = data;
        pthread_spin_init(&(node->lock), 0);
    }

    return node;
}


/**
 * Deletes a node.
 *
 * @param node       node to be freed
 */
void
free_node(struct bst_node* node) 
{
    if (node == NULL)
        fprintf(stderr, "Invalid node\n");
    else {
        /* TODO: Finalize any per node variables you use for the BST */
        free(node);
    }
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * c-file-style: "stroustrup"
 * End:
 */
